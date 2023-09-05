##########################################################################################
CLI
##########################################################################################

The command line interface (CLI) to Booleguru is the main method of interacting
with the program. Use it to compose formulas and to apply transformations.

Full Grammar of the Expression Language
---------------------------------------

The CLI expression language is defined through an ANTLR4 grammar which is given
in full below.

.. a4:autogrammar:: cli_parser
   :only-reachable-from: cli_parser.invocation
   :undocumented:
   :cc-to-dash:

.. a4:autogrammar:: cli_lexer
   :undocumented:
   :cc-to-dash:
