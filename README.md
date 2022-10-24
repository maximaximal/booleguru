# Booleguru

## Relation between Common Lisp and C++

C++ is used to build core structures and mechanisms, while Common Lisp is used
to enhance C++ and provide better structural capabilities than easily
describable using C++. Implementation is therefore split between C++ and Common
Lisp, compile-time compiled to C.

### Why Even Include Common Lisp

  1. Nice matching against structures (high level formula optimizing)
  2. Easy and efficient parser for SMT-LIB
  3. Native scripting language, making some features easier to implement
     (mainly complex formula traversal)
  4. Provide a socket to connect to to debug traversal things. This should be
     the debugging mode default.

Common Lisp should be both embeddable and run `.cl` files. This would make the
internals scriptable by common lisp and then compilable into a static binary
for release. During debugging, one could start a swank server at any point.

## Structure

This document describes how the different components of booleguru interact with
each other.

### Expression manipulation core `booleguru::expression`

Optimized expression management. This is the very core of booleguru.

### Expression manipulation core `booleguru::transform`

Optimized expression transformations, optimizations, etc..

### Parsing input files `booleguru::parse`

Parses inputs (qcir, qdimacs, smtlib, etc). Produces expressions and/or output.

### Solve / Evaluate / Execute Formulas `booleguru::solve`

Call a SAT solver to actually solve a formula. Could also include QBF solvers
in the future or become it's own solver at some point.

### Embedded Common Lisp `booleguru::cl`

This is the part that hosts the embedded common lisp code. Stuff that is
actually embedded in cl is included here, contained in this namespace, to
clearly mark lispy code. If performance of some transformation becomes a
problem, one can move implementations. Stuff is imported into other namespaces
with `using` or by importing namespaces (e.g. `booleguru::cl::transform`).

### Internal API `booleguru::api`

Used to bind multiple languages into booleguru. Uses stuff from everywhere.
Also used to bind runtime common lisp and to provide the API for use with JS.
