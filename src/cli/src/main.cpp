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
  cout << "booleguru - boolean format multitool\n";
  cout << "\n";
  cout << "DOCUMENTATION:\n";
  cout << "  See web-based documentation at:\n";
  cout << "  https://booleguru.pages.sai.jku.at/booleguru/\n";
  cout << "QUICK USAGE EXAMPLE:\n";
  cout << "  ./booleguru [parser-args] <formula> [output-args]\n";
  cout << "\n";
  cout << "QUICK CLI EXPRESSION USAGE EXAMPLE:\n";
  cout << "  ./booleguru <unop> [ file1.boole <binop> file2.boole ]\n";
  cout << "\n";
  cout << "SUPPORTED INPUT FORMATS:\n";
  cout << "  (Q)DIMACS\n";
  cout << "  Infix Logic Format (Limboole-esque)\n";
  cout << "\n";
  cout << "SUPPORTED OUTPUT FORMATS (as [output-args]):\n";
  cout << "  --dimacs, --qdimacs\n";
  cout << "  --qcir\n";
  cout << "SUPPORTED OPERATIONS (append after some CLI expression):\n";
  cout << "  :unquantified\n";
  cout << "  :tseitin\n";
  cout << "  :distribute-to-cnf\n";
  cout << "  :solve\n";
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
main(int argc, char* argv[]) {
  if(argc > 1) {
    std::string_view arg1 = argv[1];
    if(arg1 == "--help") {
      print_help();
      return EXIT_SUCCESS;
    } else if(arg1 == "--version") {
      print_version();
      return EXIT_SUCCESS;
    }
  }

  // Preliminary stuff, just for testing!!
  cli::cli_processor cli(argc, argv);

  auto result = cli.process();

  argument::input_types t =
    std::get<argument::input_types>(cli.output_arg(argument::type));
  switch(t) {
    case cli::argument::qcir: {
      serialize::qcir qcir_out(std::cout);
      qcir_out(result);
      return EXIT_SUCCESS;
    }
    case cli::argument::boole:
      std::cout << result << std::endl;
      return EXIT_SUCCESS;
    case cli::argument::qdimacs: {
      if(result->is_cnf) {
        transform::output_to_qdimacs o(std::cout);
        o.serialize_cnf_op(result);
      } else {
        transform::tseitin<transform::output_to_qdimacs> qdimacs(std::cout);
        qdimacs(result);
      }
      return EXIT_SUCCESS;
    }
    case cli::argument::smtlib2: {
      serialize::smtlib2 s(std::cout);
      s(result);
      return EXIT_SUCCESS;
    }
    case cli::argument::none:
      return EXIT_SUCCESS;
    default:
      std::cerr << "Unsupported output type." << std::endl;
  }

  return EXIT_SUCCESS;
}
