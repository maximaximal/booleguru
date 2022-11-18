#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <iomanip>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/serialize/qcir.hpp>

#include <booleguru/cli/cli-processor.hpp>

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
  booleguru::cli::cli_processor cli(argc, argv);

  auto result = cli.process();

  booleguru::serialize::qcir qcir_out(std::cout);
  qcir_out(result);

  return EXIT_SUCCESS;
}
