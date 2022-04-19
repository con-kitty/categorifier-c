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

- `f` is monomorphic
- `Input` and `Output` are supported types

A supported type is defined as one of the following:

- A primitive type. All primitive types except `Bool` need to be wrapped in `Categorifier.C.KTypes.C`.
  Specifically, the primitive types are `Bool`, `C Int8`, `C Int16`, `C Int32`,
  `C Int64`, `C Word8`, `C Word16`, `C Word32`, `C Word64`, `C Float`, and `C Double`.
  - The `C` here is a newtype wrapper. The reason most primitive types need to be wrapped is
    because some of their default Haskell instances behave differently than the corresponding
    C functions. As an example, in Haksell, `min @Double 0.0 (-0.0) == 0.0`, while in C,
    `fmin(0.0, -0.0) == -0.0`.
- A product type where each field is a supported type
- An enum type (i.e., a sum type whose data constructors are all nullary) represented by `KEnum`.
- A `Maybe` type represented by `KMaybe`. See [examples/sum-types](examples/sum-types) for an
  example of using `KEnum` and `KMaybe`.

`Input` and `Output` are allowed to be instantiations of polymorphic types, for example
`Data.Semigroup.Product (C Int32)`.

The internal types involved in the definition of `f` are not subject to these constraints.
For example, the body of `f` is free to call another function which consumes or
produces lists, or is polymorphic.

### Required Instances

The `Input` and `Output` types should also have the following type class and
type family instances:

- `Categorifier.C.CExpr.Cat.TargetOb.TargetOb`
- `Categorifier.C.CTypes.CGeneric.Class.CGeneric`
- `Categorifier.C.CTypes.GArrays.GArrays`
- `Categorifier.Client.HasRep`
- `GHC.Generics.Generic`

The internal types usually only need `HasRep` and `TargetOb` instances.

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

## Separate categorification

Compiling large functions is slow and memory intensive in most compiled languages.
The same holds true for categorifying large function using categorifier, which is also both
slow and memory intensive. It is thus often desirable to categorify a large function in
smaller chunks. To do so, use `Categorifier.Categorify.separately` to categorify
the subparts. Each call to `separately` creates a `Categorifier.Category.NativeCat`
instance. For example, calling `separately` on function `f :: InputF -> OutputF` in
module `M` produces `instance NativeCat C.Cat "M.f" InputF OutputF`, which contains
a method `nativeK :: C.Cat InputF OutputF`. Then, if the plugin encounters `M.f`, it
does not need to inline it, but can directly replace it with `nativeK`.

An example can be found at [examples/separate-categorification](examples/separate-categorification).

## Extending MakerMap

Coming soon

## Calling External C Functions

Coming soon

## Contributing

There are compatible [direnv](https://direnv.net/) and [Nix](https://nixos.org/manual/nix/stable/) environments in the repo to make it easy to build, test, etc. everything with consistent versions to help replicate issues.

This repo is all formatted using [Ormolu](https://github.com/tweag/ormolu) (included in the direnv and Nix environments), see the [usage notes](https://github.com/tweag/ormolu#usage) for how to best integrate it with your workflow. But don't let Ormolu get in the way of contributing - CI will catch the formatting, and we can help clean up anything.
