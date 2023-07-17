#include <iostream>

#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>

#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <smtlib2_lexer.h>
#include <smtlib2_parser.h>

namespace booleguru::parse {
result
smtlib2::operator()() {
  return error("Could not get error or op from SMTLIB!");
}
}
