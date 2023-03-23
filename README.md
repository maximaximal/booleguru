# Booleguru

A symbolic logical calculator that can not only give results, but also transform
what it reads. Supports multiple input- and output formats.

This is still very early in development and everything may change. We are very
open for inputs and requests!

Presented at MBMV'23 in Freiburg
([Presentation](http://maximaximal.pages.sai.jku.at/mbmv23/), [Workshop
Website](https://cca.informatik.uni-freiburg.de/mbmv23/)).

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

### Lua and Fennel `booleguru::lua`

[Lua 5.4.4](https://www.lua.org/) together with
[Fennel](https://fennel-lang.org/) is embedded in the resulting binary. This
enables users to generate more complex formulas.

## License

MIT-License
