#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/serialize/qcir.hpp>

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
  if(argc != 2) {
    std::cerr << "REQUIRE FILE" << std::endl;
    return EXIT_FAILURE;
  }

  std::ifstream in_file(argv[1]);
  booleguru::parse::boole boole_in(in_file);
  auto in_op = boole_in();
  if(!in_op) {
    std::cerr << "Could not parse in! Message: " << in_op.message << std::endl;
    return EXIT_FAILURE;
  }
  booleguru::serialize::qcir qcir_out(std::cout);
  qcir_out(*in_op);

  return EXIT_SUCCESS;
}
