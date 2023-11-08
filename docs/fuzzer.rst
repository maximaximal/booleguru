##########################################################################################
Fuzzer
##########################################################################################

Booleguru can be combined with `LLVM's libFuzzer
<https://llvm.org/docs/LibFuzzer.html>`. Two modes are available: CLI fuzzing
(where libFuzzer is generating inputs which are put through the command line
interface) and structural fuzzing, where you supply your command line through
``BOOLEGURU_ARGS="fuzz :some-op"`` as an environment variable, which is then
processed by Booleguru.

In order to build with fuzzing enabled, use the *Clang* compiler and the *Fuzz*
CMake target. E.g. by creating a ``build-fuzz`` directory and from there executing CMake
with ``CC=clang CXX=clang++ cmake .. -DCMAKE_BUILD_TYPE=fuzz``. After building
using ``make``, the binary ``booleguru-fuzz`` is the entry point to the fuzzer.
Supply some directory as the first argument to booleguru-fuzz to store found
valid inputs.

With fuzzing, more modern versions of libFuzz support providing ``-fork=X`` as
an option. This makes the fuzzer fork into X sub-processes (provide your own
number!) to speed-up its fuzzing effort and increase executions per second.

CLI Fuzzing
-----------

This is the conceptually easier fuzzing variant. LibFuzzer generates CLI input
using its integrated mutators and just parses it with the CLI processor. Only
inputs that are valid UTF-8 and that don't arrive at an unsupported exception
are stored to the corpus.

Example::

  mkdir cli-corpus
  ./booleguru-fuzz cli-corpus

Structural Fuzzing
------------------

Structural fuzzing uses a mutator inspired by `Google's fuzzer for Protocol
Buffers <https://github.com/google/libprotobuf-mutator>`. It randomly introduces
mutations to an array of ``op`` structs and directly uses that as input to an
``op_manager``. This fuzzing variant is highly efficient and useful to test
Booleguru transformations and features.

Example::

  mkdir structural-corpus
  BOOLEGURU_ARGS="fuzz :prefixtract" ./booleguru-fuzz structural-corpus >/dev/null
  ./booleguru-print-corpus structural-corpus/*

In this example, the standard output is redirected to ``/dev/null``, because it
is not required for fuzzing.

The ``booleguru-print-corpus`` utility tool also supports the base64 encoded
output of libFuzzer crash reports as an argument and prints the equivalent
boolean formula.
