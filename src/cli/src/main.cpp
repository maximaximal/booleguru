#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/serialize/qcir.hpp>
#include <booleguru/serialize/smtlib2.hpp>
#include <booleguru/util/type.hpp>

#include <booleguru/transform/output_to_qdimacs.hpp>
#include <booleguru/transform/tseitin.hpp>

#include <booleguru/cli/cli-processor.hpp>

using namespace booleguru;
using namespace booleguru::cli;

/* The idea for the CLI of booleguru is a CLI expression parser. One should be
 * able to do similar things to the boole interface and just combine multiple
 * files, invert them, etc. So we have a collection of operations:
 *
 * 1. Input files (of some format, specified by --format=,-f= before a file)
 * 2. Connecting operations on the expressions from files (regular old CLI-args
 * like --and)
 * 3. The output file (of some format specified by --format=,-f= at the end)
 *
 * One can e.g. transform stdin to qcir by `./booleguru -f qcir` because stdin
 * is the default file.
 */

static void
print_help() {
  using std::cout;
  cout << "booleguru - boolean format multitool, propositional polyglot "
          "- " BOOLEGURU_VERSION "\n";
  cout << "\n";
  cout << "DOCUMENTATION:\n";
  cout << "  See web-based documentation at:\n";
  cout << "  https://booleguru.pages.sai.jku.at/booleguru/\n";
  cout << "QUICK USAGE EXAMPLE:\n";
  cout << "  booleguru [parser-args] <formula> [output-args]\n";
  cout << "\n";
  cout << "QUICK CLI EXPRESSION USAGE EXAMPLES (** is the last expression):\n";
  cout << "  booleguru <unop> [ file1.boole <binop> file2.boole ]\n";
  cout << "  booleguru test.boole \":(b_and ** (b_not (solve **)))\" :solve\n";
  cout << "  booleguru \"test.boole :rename@a@aa <-> test.boole\" --qcir\n";
  cout << "\n";
  cout << "SUPPORTED INPUT FORMATS:\n";
  cout << "  (Q)DIMACS\n";
  cout << "  Infix Logic Format (Limboole-esque)\n";
  cout << "  QCIR\n";
  cout << "  SMT-LIB2\n";
  cout << "  AIGER (AAG)\n";
  cout << "\n";
  cout << "SUPPORTED OUTPUT FORMATS (as [output-args]):\n";
  cout << "  --boole\n";
  cout << "  --dimacs, --qdimacs\n";
  cout << "  --qcir\n";
  cout << "  --smtlib\n";
  cout << "\n";
  cout << "SUPPORTED COLON OPERATORS (append after some CLI expression):\n";
  cout << "    The @ separates arguments. You may also write these using "
          "\":(func arg1)\"\n";
  cout << "    [] marks optional arguments in this list.\n";
  cout << "  :unquantified\n";
  cout << "  :dotter@[path]\n";
  cout << "  :quantlist\n";
  cout << "  :prefixtract\n";
  cout << "  :quantblocks\n";
  cout << "  :tseitin\n";
  cout << "  :eliminate-implication\n";
  cout << "  :eliminate-equivalence\n";
  cout << "  :eliminate-xor\n";
  cout << "  :distribute-ors\n";
  cout << "  :distribute-to-cnf\n";
  cout << "  :solve\n";
  cout << "  :rename@A@B\n";
  cout << "  :linearize-quants-{E,A}{up,down}-{up-down}\n";
  cout << "  :linearize-quants-legacy-E{up,down}-{up-down}\n";
  cout << "  :counterfactuals (see docs)\n";
  cout << "  :eqkbf (see docs)\n";
  cout << "\n";
  cout << "BINARY OPERATORS (between two expressions):\n";
  cout << "  no supported binary operators at the moment\n";
  cout << "  implement fennel scripts using the global *l* and *r*\n";
  cout << "  or lua scripts using the global left_op and right_op\n";
  cout << "\n";
  cout << "LUA and FENNEL:\n";
  cout << "  You can write Fennel code in the CLI, e.g. :(print \"hello "
          "world\")\n";
  cout << "  See the Fennel guide: https://fennel-lang.org/reference\n";
  cout << "  Set the following environment variables to add new scripts:\n";
  cout << "    BOOLEGURU_LUA_PATH for .lua scripts\n";
  cout << "    BOOLEGURU_FENNEL_PATH for .fnl scripts\n";
  cout << "  Scripts from standard Lua or Fennel directories are also loaded.\n";
  cout << "  Scripts are loaded automatically if called using a colon op.\n";
  cout << std::endl;
}

#define xstr(s) str(s)
#define str(s) #s

static void
print_version() {
  using std::cout;
  cout << BOOLEGURU_VERSION << std::endl;
}

#undef xstr
#undef str

int
main(int argc, const char* argv[]) {
  if(argc > 1) {
    std::string_view arg1 = argv[1];
    if(arg1 == "--help" || arg1 == "-h" || arg1 == "-?" || arg1 == "--h") {
      print_help();
      return EXIT_SUCCESS;
    } else if(arg1 == "--version") {
      print_version();
      return EXIT_SUCCESS;
    }
  }

  if(argc == 1) {
    print_help();
    return EXIT_SUCCESS;
  }

  cli::cli_processor cli(argc, argv);

  auto result = cli.process();

  using enum util::type;

  util::type t = cli.output_type();
  switch(t) {
    case qcir: {
      serialize::qcir qcir_out(std::cout);
      qcir_out(result);
      return EXIT_SUCCESS;
    }
    case boole:
      std::cout << result << std::endl;
      return EXIT_SUCCESS;
    case qdimacs: {
      if(result->is_cnf) {
        transform::output_to_qdimacs o(std::cout);
        o.serialize_cnf_op(result);
      } else {
        transform::tseitin<transform::output_to_qdimacs> qdimacs(std::cout);
        qdimacs(result);
      }
      return EXIT_SUCCESS;
    }
    case smtlib: {
      serialize::smtlib2 s(std::cout);
      s(result);
      return EXIT_SUCCESS;
    }
    case none:
      return EXIT_SUCCESS;
    default:
      std::cerr << "Unsupported output type." << std::endl;
  }

  return EXIT_SUCCESS;
}
