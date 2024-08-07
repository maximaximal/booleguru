# Booleguru

Booleguru is a propositional polyglot! It provides a framework for
efficiently working with logical formulas that may contain
quantifiers.

  - Presentation at IJCAR'24 in Nancy
    [Slides](https://general-public-talks.pages.sai.jku.at/ijcar-2024/)
  - Presentation at the MBMV'23 in Freiburg
    ([Presentation](http://maximaximal.pages.sai.jku.at/mbmv23/),
    [Workshop
    Website](https://cca.informatik.uni-freiburg.de/mbmv23/)).
  - [Documentation](https://booleguru.pages.sai.jku.at/booleguru/).

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

## Citing

Please cite us when you use our tool for conversions or as a library! We are
happy to hear about your use-cases. :)

```
@InProceedings{10.1007/978-3-031-63498-7_19,
author="Heisinger, Maximilian
and Heisinger, Simone
and Seidl, Martina",
editor="Benzm{\"u}ller, Christoph
and Heule, Marijn J.H.
and Schmidt, Renate A.",
title="Booleguru, the Propositional Polyglot (Short Paper)",
booktitle="Automated Reasoning",
year="2024",
publisher="Springer Nature Switzerland",
address="Cham",
pages="315--324",
isbn="978-3-031-63498-7"
}
```
