#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/serialize/qcir.hpp>

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

int
main(int argc, char* argv[]) {

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
    case cli::argument::none:
      return EXIT_SUCCESS;
    default:
      std::cerr << "Unsupported output type." << std::endl;
  }

  return EXIT_SUCCESS;
}
