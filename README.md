# Booleguru

A symbolic logical calculator that can not only give results, but also transform
what it reads. Supports multiple input- and output formats.

This is still very early in development and everything may change. We are very
open for inputs and requests!

## Structure

This document describes how the different components of booleguru interact with
each other.

### Expression manipulation core `booleguru::expression`

Optimized expression management. This is the very core of booleguru.

### Expression manipulation core `booleguru::transform`

Optimized expression transformations, optimizations, etc..

### Parsing input files `booleguru::parse`

Parses inputs (qcir, qdimacs, smtlib, etc). Produces expressions and/or output.

Inline-Lisp: Not possible, when no operators have to be between expressions. If
I force operators everywhere, I cannot do meta-definitions. I would have to
include other files using some other mechanism. With purely inline lisp, I'd put
some operator somewhere, and as soon as the operator is missing, parsing breaks
and lisp is executed. I'd have to define a sort of extra-syntax using e.g. []
that is then run as a program and that is outside the expression tree.

Counter example: (print (var 'a)). Is (var 'a) an expression that should be run
on it's own and print just some variable, or is (print (var 'a)) the expression?

It would still be possible to just force operators and if they are missing I
break out. I'll do that.

### Solve / Evaluate / Execute Formulas `booleguru::solve`

Call a SAT solver to actually solve a formula. Could also include QBF solvers
in the future or become it's own solver at some point.

### Embedded Common Lisp `booleguru::cl`

This is the part that hosts the embedded common lisp code. Stuff that is
actually embedded in cl is included here, contained in this namespace, to
clearly mark lispy code. If performance of some transformation becomes a
problem, one can move implementations. Stuff is imported into other namespaces
with `using` or by importing namespaces (e.g. `booleguru::cl::transform`).

# Live Lisp Development

The common lisp part is also compiled info a `fasb` file that can be loaded from
ECL. To use it, run the following expression in a running ECL REPL with the
corrected path:

```
(load #p "~/sai/booleguru/build-clang/booleguru-cl.fasb")
```

To then enable live editing of the environment, load all functions into the ECL
REPL and override at will. Non-overridden functions stay implemented inside the
`fasb` file.

## License

MIT-License
