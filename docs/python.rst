##########################################################################################
Python
##########################################################################################

Booleguru can be used twofold from Python: either you `import pybooleguru` and
use it just as you would use `Z3Py
<https://ericpony.github.io/z3py-tutorial/guide-examples.htm>`, or you generate
formulas inside of your Python script and use the script as input file using
booleguru's CLI.

The `pybooleguru` Module
------------------------

The API is inspired by `Z3Py
<https://ericpony.github.io/z3py-tutorial/guide-examples.htm>`, with some
limitations and extensions.

Python Input Files
------------------

You can use the same API as you would when using `pybooleguru`, but you have to
set the global variable `next_op` at the end. This is then given back into the
CLI processor to be then used by later stages in your boolean pipeline.

Example for the Python API
--------------------------

Below you find an example of how you can already use `pybooleguru`:

.. code-block:: python

    from pybooleguru import *
    a = Bool("a")
    b = Bool("b")
    c = Bool("c")
    f = Or(And(a, b), c)
    s = Solver()
    s.add(f)
    s.add(Not(c))
    s.add(Not(a))
    res = s.check()
    m = s.model()
    if res == result.SAT:
	print(f"a:{m[a]}, b:{m[b]}, c:{m[c]}")
    else:
	print("Unsat")
