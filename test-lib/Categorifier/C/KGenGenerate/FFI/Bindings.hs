module Categorifier.C.KGenGenerate.FFI.Bindings
  ( mkFFIBindingModule,
    ffiBindingModule,
  )
where

import Categorifier.C.Codegen.FFI.Spec (SBVSpec)
import Categorifier.C.KGenGenerate.FFI.Spec
  ( inputSpecFunName,
    inputSpecSizeFunName,
    outputSpecFunName,
    outputSpecSizeFunName,
  )
import Data.Functor (void)
import Language.Haskell.Exts.Build (name, pvar, var)
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Pretty as Pretty
import Language.Haskell.Exts.Syntax
  ( CallConv (..),
    Decl (..),
    Exp (..),
    ExportSpec (..),
    ExportSpecList (..),
    FieldUpdate (..),
    ImportDecl (..),
    ImportSpec (..),
    ImportSpecList (..),
    Literal (..),
    Module (..),
    ModuleHead (..),
    ModuleName (..),
    ModulePragma (..),
    Namespace (..),
    QName (..),
    Rhs (..),
    Safety (..),
    Type (..),
  )
import qualified Text.Casing as Casing

mkFFIBindingModule :: String -> SBVSpec -> String
mkFFIBindingModule funName = Pretty.prettyPrint . ffiBindingModule funName

ffiBindingModule :: String -> SBVSpec -> Module ()
ffiBindingModule funName spec = Module () hd prags imports decls
  where
    hd =
      Just $
        ModuleHead
          ()
          (ModuleName () . Casing.pascal $ funName)
          Nothing
          exports
    prags = [LanguagePragma () [name "ForeignFunctionInterface"]]
    imports =
      [ ImportDecl
          ()
          (ModuleName () "Categorifier.C.Codegen.FFI.Spec")
          False
          False
          False
          Nothing
          Nothing
          ( Just . ImportSpecList () False $
              [ IAbs () (NoNamespace ()) (name "SBVGetSpecSize"),
                IAbs () (NoNamespace ()) (name "SBVGetSpec"),
                IAbs () (NoNamespace ()) (name "SBVFunCall"),
                IThingAll () (name "Spec"),
                IThingAll () (name "SBVCFunction")
              ]
          ),
        ImportDecl
          ()
          (ModuleName () "Categorifier.C.Prim")
          False
          False
          False
          Nothing
          Nothing
          ( Just . ImportSpecList () False $
              [ IThingAll () (name "Arrays")
              ]
          ),
        ImportDecl
          ()
          (ModuleName () "Categorifier.C.PolyVec")
          False
          False
          False
          Nothing
          Nothing
          ( Just . ImportSpecList () False $
              [ IThingAll () (name "ArrayCount")
              ]
          )
      ]
    decls =
      ffiDecls funName
        <> [ structTypeDecl funName,
             structDecl funName spec
           ]
    exports =
      Just . ExportSpecList () . pure
        . EVar ()
        . UnQual ()
        . name
        $ haskellName id funName

mkFFIDecl :: (String, String, Type ()) -> Decl ()
mkFFIDecl (funName, cFunName, funTy) =
  ForImp
    ()
    (CCall ())
    (Just $ PlayRisky ())
    (Just cFunName)
    (name funName)
    funTy

ffiDecls :: String -> [Decl ()]
ffiDecls funName =
  fmap
    (mkFFIDecl . tyConify)
    [ ( inputSpecSizeFunName funName,
        inputSpecSizeFunName funName,
        "SBVGetSpecSize"
      ),
      ( outputSpecSizeFunName funName,
        outputSpecSizeFunName funName,
        "SBVGetSpecSize"
      ),
      ( inputSpecFunName funName,
        inputSpecFunName funName,
        "SBVGetSpec"
      ),
      ( outputSpecFunName funName,
        outputSpecFunName funName,
        "SBVGetSpec"
      ),
      (haskellName id funName <> "'", funName, "SBVFunCall")
    ]
  where
    tyConify (x, y, tyName) = (x, y, TyCon () . UnQual () $ name tyName)

haskellName :: (String -> String) -> String -> String
haskellName f = Casing.camel . f . Casing.quietSnake

structTypeDecl :: String -> Decl ()
structTypeDecl funName =
  TypeSig () [name $ haskellName id funName] . TyCon () . UnQual () $ name "SBVCFunction"

structDecl :: String -> SBVSpec -> Decl ()
structDecl funName spec =
  PatBind () (pvar . name $ haskellName id funName) rhs Nothing
  where
    rhs =
      UnGuardedRhs () . RecConstr () (UnQual () $ name "SBVCFunction") $
        fmap
          (uncurry $ FieldUpdate ())
          [ ( UnQual () $ name "sbvCInputSpecSize",
              var . name $ inputSpecSizeFunName funName
            ),
            ( UnQual () $ name "sbvCOutputSpecSize",
              var . name $ outputSpecSizeFunName funName
            ),
            ( UnQual () $ name "sbvCInputSpec",
              var . name $ inputSpecFunName funName
            ),
            ( UnQual () $ name "sbvCOutputSpec",
              var . name $ outputSpecFunName funName
            ),
            ( UnQual () $ name "sbvCFunCall",
              var . name $ haskellName id funName <> "'"
            ),
            (UnQual () $ name "sbvCFunName", Lit () $ String () funName funName),
            (UnQual () $ name "sbvCFunSpec", outspec)
          ]
    outspec = case Parser.parseExp (show spec) of
      Parser.ParseFailed loc msg ->
        error $ "failure parsing SBV spec at '" <> show loc <> "': " <> msg
      Parser.ParseOk spexpr -> void spexpr
