{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Utilities for running actions in subprocesses and receiving information back from them.
--
-- This is used in plugin tests to run generated C code in a separate process and correctly handle
-- things like SIGFPE, where the process running the code will get killed immediately (and can't
-- handle it because it results in an infinite signal stream).
module Categorifier.C.KGenGenerate.Test.Subprocess
  ( Channel,
    Uniplex (..),
    SubprocessError (..),
    sendToPipe,
    receiveFromPipe,
    runSubprocessWithChannel,
  )
where

import qualified Categorifier.Common.IO.Exception as Exception
import Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise (DeserialiseFailure, deserialiseOrFail, serialise)
import Control.Exception (IOException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except (runExceptT, throwE)
import qualified Data.Binary as Binary (decodeOrFail, encode)
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS (fromStrict, toStrict)
import qualified System.Exit as Exit
import qualified System.IO as IO (Handle, hClose)
import qualified System.Posix.IO as IO (createPipe, fdToHandle)
import qualified System.Posix.Process as Process
import qualified System.Posix.Types as IO (Fd)

data SubprocessError
  = Exception IOException
  | DecodeError Serialise.DeserialiseFailure
  | DecodeLengthError String
  | SubprocessExit Process.ProcessStatus
  | WTF
  deriving (Show)

data Uniplex
  = Tx
  | Rx

newtype Channel (direction :: Uniplex) a = Channel {_getChannelPipe :: IO.Handle}

createPipe :: IO (Channel 'Rx a, Channel 'Tx a)
createPipe = IO.createPipe >>= bitraverse convertFd convertFd
  where
    convertFd :: IO.Fd -> IO (Channel direction a)
    convertFd = fmap Channel . IO.fdToHandle

closePipe :: Channel direction a -> IO ()
closePipe (Channel handle) = IO.hClose handle

sendToPipe :: Serialise a => a -> Channel 'Tx a -> IO ()
sendToPipe value (Channel txHandle) = do
  BS.hPut txHandle lengthEncoded
  BS.hPut txHandle valueSerialized
  where
    -- This is all a bit of a pain.  Unix pipes don't really provide metadata about message size;
    -- they're just streams.  'Serialise.serialise' doesn't result in constant-size encoding for
    -- integers, so we have to use 'Serialise.serialise' (which we already have instances of) to
    -- serialise the user message, then use 'Binary.encode' to encode the length of the message,
    -- send that, and finally send the user data.

    -- The serialized data to be sent
    valueSerialized = BS.toStrict $ Serialise.serialise value
    -- The length of the serialized data
    valueSerializedLength = BS.length valueSerialized
    -- The length of the serialized data, encoded via Data.Binary for transmission over the pipe
    lengthEncoded = BS.toStrict $ Binary.encode valueSerializedLength

receiveFromPipe ::
  forall a.
  Serialise a =>
  Channel 'Rx a ->
  IO (Either SubprocessError a)
receiveFromPipe (Channel rxHandle) =
  Except.runExceptT $
    -- First read the length of the incoming message (whose length itself is known)
    readFromPipe lengthSize
      >>= decodeOrFailBinary
      >>=
      -- Now read the actual message
      readFromPipe
      >>= decodeOrFailTest
  where
    -- The length of any message containing the length of a ByteString encoded via Data.Binary.
    lengthSize = BS.length . BS.toStrict . Binary.encode $ BS.length BS.empty
    -- Read the specified number of bytes from the handle
    readFromPipe = liftIO . BS.hGet rxHandle
    -- Decode a Data.Binary-encoded message
    decodeOrFailBinary :: BS.ByteString -> ExceptT SubprocessError IO Int
    decodeOrFailBinary bytes =
      case Binary.decodeOrFail $ BS.fromStrict bytes of
        Left (_, _, err) -> Except.throwE $ DecodeLengthError err
        Right (_, _, v) -> pure v
    -- Decode a Codec.Serialise-encoded message
    decodeOrFailTest :: BS.ByteString -> ExceptT SubprocessError IO a
    decodeOrFailTest bytes =
      case Serialise.deserialiseOrFail $ BS.fromStrict bytes of
        Left err -> Except.throwE $ DecodeError err
        Right value -> liftIO $ Exception.evaluate value

runSubprocessWithChannel ::
  -- | Parent process action (receives messages)
  (Channel 'Rx a -> IO (Either SubprocessError b)) ->
  -- | Child process action (sends messages)
  (Channel 'Tx a -> IO ()) ->
  -- | Result from the parent process after the child process has returned.
  IO (Either SubprocessError b)
runSubprocessWithChannel parentAction childAction = do
  pipe@(rxChannel, txChannel) <- createPipe
  childPid <- Process.forkProcess $ child pipe
  closePipe txChannel
  parentResult <- Exception.handle captureIOException $ parentAction rxChannel
  childStatus <- liftIO $ Process.getProcessStatus True False childPid
  closePipe rxChannel
  pure $ handleStatus childStatus parentResult
  where
    child (rxChannel, txChannel) = do
      closePipe rxChannel
      childAction txChannel
      closePipe txChannel
      Exit.exitSuccess
    captureIOException :: IOException -> IO (Either SubprocessError a)
    captureIOException = pure . Left . Exception
    handleStatus (Just (Process.Exited Exit.ExitSuccess)) result = result
    handleStatus (Just x) _ = Left $ SubprocessExit x
    handleStatus Nothing _ = Left WTF
