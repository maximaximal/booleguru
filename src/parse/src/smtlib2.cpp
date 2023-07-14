#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>

#include <iostream>

namespace booleguru::parse {
result
smtlib2::operator()() {
  return error("Could not get error or op from SMTLIB!");
}
}
