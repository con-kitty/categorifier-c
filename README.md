# C Backend for Categorifier ![Build status](https://github.com/con-kitty/categorifier-c/actions/workflows/ci.yml/badge.svg?branch=master)

This repo is a backend for the [categorifier plugin](https://github.com/con-kitty/categorifier), with the purpose of compiling Haskell
to C. It contains a cartesian closed category,
referred to as CCat, as well as code that converts morphisms in this category into C code.

At a high level, a Haskell function is compiled into a C function via the following steps:

- The categorifier plugin (i.e., frontend) maps the Hask morphism (i.e., the Haskell function)
  into a CCat morphism.
  - Hask is, roughly speaking, a category where objects are Haskell types and morphisms are
    Haskell functions.
  - CCat is a category where objects are Haskell types, and a morphism from `A` to `B` is a
    Haskell function from `TargetOb A` to `TargetOb B`. The frontend is therefore a functor
    from Hask to CCat, whose action on objects is identity.
- The backend converts `TargetOb A -> TargetOb B` into a C function.

The main advantages of compiling Haskell into C this way, compared to the common
alternative approach of using an embedded DSL, are:

- It allows the source Haskell code to be normal, vanilla Haskell. An eDSL-based
  approach usually imposes many restrictions on how the source code is written
  (for example, many functions and operators in `base` cannot be used).
- It can potentially generate more readable C code. This is WIP, but a compiler
  plugin, being part of the compilation pipeline, has convenient access to
  information such as variable and function names, module names, function boundaries
  and so on, which can be taken advantage of to produce readable C code.
- It generates smaller and more efficient C code. We've compared the plugin and
  an eDSL based approach on the same Haskell code, and the plugin produces slightly
  smaller C code due to certain optimizations it has the opportunities to perform.

On the flip side, using a compiler plugin is more fragile than an eDSL-based
approach, and it is harder to support multiple GHC versions. Also, it is harder to give
a precise, formal description of the subset of Haskell that is supported. That being said, this
approach has been tested extensively at [Kittyhawk](https://www.kittyhawk.aero/) on its flight
controller codebase. The stability turns out to be quite good, and the ability for control engineers
to write normal Haskell is highly appreciated.

The readers are encouraged to refer to the [examples](examples) to follow along this document.

## Supported Haskell Types

To be able to compile a haskell function `f :: Input -> Output` into a C function, the
following conditions must be met:

- `f` is monomorphic (or a polymorphic function whose type variables are all instantiated)
- `Input` and `Output` have the following instances:
  - `Categorifier.C.CExpr.Cat.TargetOb.TargetOb`
  - `Categorifier.C.CTypes.CGeneric.Class.CGeneric`
  - `Categorifier.C.CTypes.GArrays.GArrays`
  - `Categorifier.Client.HasRep`
  - `GHC.Generics.Generic`

Currently, these instances can be defined for the following types:

- Primitive types. All primitive types except `Bool` need to be wrapped in `Categorifier.C.KTypes.C`.
  Specifically, the primitive types are `Bool`, `C Int8`, `C Int16`, `C Int32`,
  `C Int64`, `C Word8`, `C Word16`, `C Word32`, `C Word64`, `C Float`, and `C Double`.
  - The `C` here is a newtype wrapper. The reason most primitive types need to be wrapped is
    because some of their default Haskell instances behave differently than the corresponding
    C functions. As an example, in Haksell, `min @Double 0.0 (-0.0) == 0.0`, while in C,
    `fmin(0.0, -0.0) == -0.0`.
- Product types where each field is a type that has the above instances.
- Enum types (i.e., sum types whose data constructors are all nullary) represented by `KEnum`.
- `Maybe a` types represented by `KMaybe`, where `a` has the above instances.

See [examples/sum-types](examples/sum-types) for an example of using `KEnum` and `KMaybe`.

At present, the above instances (except `HasRep` and `TargetOb`) cannot be written for
arbitrary sum types. Supporting non-recursive sum types is work-in-progress, while supporting
recursive sum types such as lists would require much greater effort.

The internal types involved in the definition of `f` are not subject to these constraints.
For example, the body of `f` may use lists or other sum types which don't have all of the above
instances. There are still some constraints though: the internal types are generally required to
have at least `HasRep` and `TargetOb` instances defined; and types used in branches
(e.g., `T` in `if b then x else y :: T`) need to have `ConCat.Category.IfCat` instances.

<!-- TODO: explain the following:
- How to write TargetOb instances
- How to use CG.AsBitfield
- ???
-->

## Producing Multiple C Functions via `kFunctionCall`

Instead of compiling a Haskell function into one big C function, it is possible to produce
multiple smaller C functions. This can be done by wrapping each part for which a separate
C function is desired with `kFunctionCall`.

For example, suppose the top-level function being compiled is `f`, which calls `g`:

```haskell
f :: InputF -> OutputF
f = ... g ...
  where
    g :: InputG -> OutputG
    g = <body>
```

To generate a separate function for `g`, just replace `g = <body>` with
`g = kFunctionCall (Proxy @C) "separate_function_name" <body>`.

Note that this imposes additional restrictions on `InputG` and `OutputG`. They must
now meet the aforementioned criteria of supported types. Previously they are considered
internal types, and do not need to meet those criteria.

Doing so can reduce the compilation time of the generated C code, since compiling large C
functions can be very slow.

An example can be found at [examples/multiple-c-functions](examples/multiple-c-functions).

## Separate Categorification

Compiling large functions is slow and memory intensive in most compiled languages.
The same holds true for categorifying large function using categorifier. It is
thus often desirable to categorify a large function in
smaller chunks. To do so, use `Categorifier.Categorify.function` to categorify
the subparts. Each call to `function` creates a `Categorifier.Category.NativeCat`
instance. For example, calling `function` on function `f :: InputF -> OutputF` in
module `M` produces `instance NativeCat C.Cat "M.f" InputF OutputF`, which contains
a method `nativeK :: C.Cat InputF OutputF`. Then, if the plugin encounters `M.f`, it
does not need to inline it, but can directly replace it with `nativeK`.

An example can be found at [examples/separate-categorification](examples/separate-categorification).

## Automatic Interpretation

The Categorifier plugin takes as input a [function that attempts to automatically interpret Haskell functions](https://github.com/con-kitty/categorifier/blob/bdecff7019e3862c49a8360d7640710902bb1e58/plugin/Categorifier/Core/Categorify.hs#L113).
Recall that Categorifier transforms a Haskell function of type `A -> B` (i.e., an arrow in the Hask category)
into an arrow in the target category, i.e., ``A `k` B`` for some `k`. If both `A -> B` and ``A `k` B`` are
instantiations of the same polymorphic function, then no work is needed to categorify it.
This is called automatic interpretation.

When using categorifier-c, `k ~ Categorifier.C.CExpr.Cat.Cat`. `Cat` is a newtype wrapper, and ``A `Cat` B``
is isomorphic to `TargetOb A -> TargetOb B`. Thus a function `A -> B` can be auto-interpreted if `A -> B`
and `TargetOb A -> TargetOb B` are the instantiations of the same polymorphic function.

An example can be found at [examples/auto-interpret](examples/auto-interpret). In this example, the type of
function `F.g` is `g :: KType1 f => f Int32 -> f Word64`. We need to categorify `g @C`, i.e., `C Int 32 -> C Int64`,
into

```haskell
TargetOb (C Int32) -> TargetOb (C Int64)
~ CExpr Int32 -> CExpr Int64
```

Both `C` and `CExpr` have `KType1` instances, which means what we need is simply `g @CExpr`. Because
`g @C` and `g @CExpr` are instantiations of the same polymorphic function `g`, no compilation
is needed. The body of `g` contains IO actions and `unsafePerformIO`, which Categorifier can't normally compile,
but in this case it doesn't matter, because `g` does not need to be inlined.

Another example of a function that can be automatically interpreted is

```haskell
Linear.Vector.^+^ :: Num a => f a -> f a -> f a
```

Since `TargetOb (Linear.V2.V2 a) = Linear.V2.V2 (TargetOb a)`, the result of categorifying `(^+^) @V2 @(C Int64)`
is simply `(^+^) @V2 @(CExpr Int64)`, and again, no compilation is needed.

To enable automatic interpretation, we need to pass an auto-interpreter corresponding to the target category
to the Categorifier plugin. An auto-interpret for the `Categorifier.C.CExpr.Cat.Cat` category is
`Categorifier.C.UnconCat.tryAutoInterpret`. To use this auto-interpreter, add the following GHC option:

```
-fplugin-opt Categorifier:autointerpreter:Categorifier.C.UnconCat.tryAutoInterpret
```

Automatic interpretation and the aforementioned separate categorification share a similar idea, but serve
different purposes. Automatic interpretation is done automatically for eligible functions, does not require
Categorifier to compile the body of the functions, and can be applied to non-exported functions.
Separate categorization is more widely applicable - it does not require
`A -> B` and ``A `k` B`` to be instantiations of the same polymorphic function, but one needs to
explicitly categorify each subpart, and thus it cannot be used on non-exported functions.

## Extending MakerMap

Coming soon

## Calling External C Functions

Coming soon

## Contributing

There are compatible [direnv](https://direnv.net/) and [Nix](https://nixos.org/manual/nix/stable/) environments in the repo to make it easy to build, test, etc. everything with consistent versions to help replicate issues.

This repo is all formatted using [Ormolu](https://github.com/tweag/ormolu). Currently CI runs Ormolu 0.4.0.0, which can be installed by `cabal install ormolu-0.4.0.0`. See the [usage notes](https://github.com/tweag/ormolu#usage) for how to best integrate it with your workflow. But don't let Ormolu get in the way of contributing - CI will catch the formatting, and we can help clean up anything.
