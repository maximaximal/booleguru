#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

#include <fmt/format.h>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/fuzz/source.hpp>

#include <booleguru/util/base64.hpp>

int
main(int argc, char** argv) {
  if(argc == 1) {
    std::cerr << "!! Require at least argument to some corpus file!"
              << std::endl;
    return EXIT_FAILURE;
  }

  for(int i = 1; i < argc; ++i) {
    fmt::println("Corpus File: {}", argv[i]);

    std::ifstream f(argv[i], std::ios_base::binary);

    std::stringstream buffer;
    if(f) {
      buffer << f.rdbuf();
    } else {
      // Otherwise, use base64 decode.
      buffer << base64::from_base64(argv[i]);
    }

    std::string input(buffer.str());

    using namespace booleguru::fuzz;
    using namespace booleguru::expression;
    op_manager ops;
    source s(ops);
    size_t inserted
      = s.apply(reinterpret_cast<const uint8_t*>(input.c_str()), input.size());

    fmt::println("  Inserted {} ops.", inserted);

    for(size_t i = 1; i <= ops.size(); ++i) {
      const op& o_ = ops.getobj(i);
      if(o_.is_binop() || o_.is_quant()) {
        fmt::println("    Op {} is type {}, left: {}, right: {}",
                     i,
                     op_type_to_str(o_.type),
                     o_.left().id_,
                     o_.right().id_);
      } else if(o_.type == op_type::Var) {
        fmt::println("    Op {} is type Var, v: {}, q: {}, i: {}",
                     i,
                     o_.var.v.id_,
                     o_.var.q,
                     o_.var.i);
      }
    }

    std::cout << "  Last op (last fuzz file): " << ops[ops.size()] << std::endl;
  }
}
