##########################################################################################
Booleguru Reference Manual
##########################################################################################

Booleguru is a multi-tool for working with Boolean formulas. You can use it to
generate, transform, solve, or generally modify Boolean formulas of various
formats. The stated goal of this project is to provide readers and writers for a
wide variety of common Boolean formulas to draw the research community together,
along with commonly needed functions to transform them. Think of it as `shell
for the world of propositional logic`.

Booleguru was presented first in the MBMV'23 workshop (`workshop page
<https://cca.informatik.uni-freiburg.de/mbmv23/>`_, `presentation
<http://maximaximal.pages.sai.jku.at/mbmv23/>`_).

Table of Contents
=================

.. toctree::
    :glob:
    :titlesonly:

    *

Usage
=====

Booleguru has multiple modes of interaction, which also complement each other.

Command Line Interface
----------------------

The first point of entry is usually the command line interface, which itself is
modelled after an infix propositional logic language. You can build expressions
from commands, files, or generators, and combine them using unary (``--not``,
``!``) or binary (``--and``, ``--or``, ``--impl``, ``--lpmi``, ``--equi``)
operators. You can serialize the output according to your needs.

Some quick examples:

- Convert a formula from infix notation to (Q)DIMACS: ``./booleguru
  formula.boole --dimacs``
- Convert a formula from infix notation to QCIR: ``./booleguru formula.boole
  --qcir``.

Please see :ref:`CLI` for a full documentation on the available arguments,
operations, and the input grammar.

Lua (and Fennel) Interface
--------------------------

Support for the `Lua <https://www.lua.org/>`_ programming language is compiled
into each Booleguru release. Some tools for formula introspection are
implemented purely in Lua and loaded on demand. If you want to write your own
generators or tools for your own workflow, it is advisable to write them in Lua
or `Fennel <https://fennel-lang.org/>`_ (a Lua-based Lisp implementation).

Python Interface
----------------

A Python interface may also be included, if the required development packages
are available during compiling. Python support is only available if you are
running Booleguru locally with a native executable, as it uses your system's
Python library. You can both import Booleguru as a Python module to work with it
there, or write scripts in Python which are executed by Booleguru to return
Boolean expressions, which can then be processed using other Booleguru tools and
scripts.

Development and Source Code
===========================

Booleguru is developed primarily in the `development branch on our GitLab
instance
<https://gitlab.sai.jku.at/booleguru/booleguru/-/tree/development?ref_type=heads>`_.
New features are first introduced there or in feature branches and then merged
with the `main branch <https://gitlab.sai.jku.at/booleguru/booleguru>`_.

During development, Booleguru's integrated fuzzer may be useful: :ref:`Fuzzer`.

In order to contribute, please see the guide for developers: :ref:`Development`.
