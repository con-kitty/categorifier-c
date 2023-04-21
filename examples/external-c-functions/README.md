# Calling External C/C++ Functions

Categorifier-C supports generating C code that calls external C/C++ functions.
This comes handy if your program cannot be implemented entirely in Haskell, and need to use a C/C++ library to do part of the work.
This document explains how to do so with a small toy example.

In the example we are going to call an external C function called `VeryUsefulFunction` which takes two integers, and returns two integers and a double: their sum, difference and average.
The code is in [very_useful_function.c](very_useful_function.c).

Let's use `/tmp/external-c-functions` as our working directory for this example.
Copy [very_useful_function.h](very_useful_function.h) and [very_useful_function.c](very_useful_function.c) to the working directory.

These are the steps needed in order to write Haskell code that can be compiled into C that interfaces with this function.

**Step 1**: define the input and output types of `VeryUsefulFunction` in Haskell.
See `CInput` and `COutput` in [CTypes.hs](CTypes.hs).
There should be a single Haskell type corresponding to the input of the C function, and a single Haskell type corresponding to the output.

**Step 2**: generate C types for these Haskell types, and helper functions for converting the C structs to/from primitive arrays. To do so, run

```
cabal run categorifier-c-examples:external-c-functions -- gen-ctypes /tmp/external-c-functions
```

This generates a bunch of C and C++ files in `/tmp/external-c-functions`.
The ones relevant to this example are `c_types.h` which contains the C structs corresponding to `CInput` and `COutput`, as well as `from_arrays.c` and `to_arrays.c` which contain functions converting `CInput` and `COutput` to/from arrays.

**Step 3**: write a C function that wraps `VeryUsefulFunction` using the array calling convention.
A function conforming to the array calling convention takes 22 primitive arrays, 11 for input and 11 for output.
It should have the following prototype:

```c
void fun_name(
    // input
    const bool *input_bool, const int8_t *input_int8_t,
    const int16_t *input_int16_t, const int32_t *input_int32_t,
    const int64_t *input_int64_t, const uint8_t *input_uint8_t,
    const uint16_t *input_uint16_t, const uint32_t *input_uint32_t,
    const uint64_t *input_uint64_t, const float *input_float,
    const double *input_double,

    // output
    bool *output_bool, int8_t *output_int8_t, int16_t *output_int16_t,
    int32_t *output_int32_t, int64_t *output_int64_t, uint8_t *output_uint8_t,
    uint16_t *output_uint16_t, uint32_t *output_uint32_t,
    uint64_t *output_uint64_t, float *output_float, double *output_double);
```

The definition of the wrapper function should construct an input struct from the input arrays, call the C function proper, and populate the output arrays from the output.
See [very_useful_function_wrapper.h](very_useful_function_wrapper.h) and [very_useful_function_wrapper.c](very_useful_function_wrapper.c) for the wrapper function for `VeryUsefulFunction`. Copy these two files to the working directory.

**Step 4**: write the Haskell function to be compiled into C.
We can obtain a function of type `CInput -> COutput` via `kForeignFunctionCall`:

```haskell
runVeryUsefulFunction :: CInput -> COutput
runVeryUsefulFunction =
  kForeignFunctionCall
    (Proxy @C)
    "Wrap_VeryUsefulFunction"
    (Imported "very_useful_function_wrapper.h")
    Nothing
```

The last argument is of type `Maybe (CInput -> COutput)`.
It lets us optionally provide a Haskell implementation of the external C function, in this case `VeryUsefulFunction`.
Providing a Haskell implementation allows us to run `runVeryUsefulFunction`, as well as functions calling it, in Haskell, which is convenient for development and debugging.
Otherwise, we can only run these functions by compiling them to C and running the C code.

Now we can use `runVeryUsefulFunction` normally in Haskell.
See `f` in [F.hs](F.hs) for example.
To compile `f` to C, run

```
cabal run categorifier-c-examples:external-c-functions -- /tmp/external-c-functions f
```

And now `cd /tmp/external-c-functions && gcc -c f.c` should succeed.
