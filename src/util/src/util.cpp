#include <iostream>

#include <booleguru/util/die.hpp>
#include <booleguru/util/istringviewstream.hpp>
#include <booleguru/util/reverse.hpp>

namespace booleguru::util {
void
die(const std::string& message) {
  std::cerr << "[Booleguru] DYING: " << message << std::endl;
  exit(1);
}
}
