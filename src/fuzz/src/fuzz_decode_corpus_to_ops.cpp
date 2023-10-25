#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

#include <fmt/format.h>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/fuzz/source.hpp>

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
    buffer << f.rdbuf();
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
      fmt::println("    Op {} is type {}, left: {}, right: {}",
                   i,
                   (int)o_.type,
                   o_.left().id_,
                   o_.right().id_);
    }

    std::cout << "  Last op (last fuzz file): " << ops[ops.size()] << std::endl;
  }
}
