#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <iostream>
#include <sstream>

#include <boole_lexer.h>
#include <boole_parser.h>

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
struct boole::internal {
  ANTLRInputStream input;
  boole_lexer lexer;
  CommonTokenStream tokens;
  boole_parser parser;

  internal(std::istream& in)
    : input(in)
    , lexer(&input)
    , tokens(&lexer)
    , parser(&tokens) {}
};
void
boole::internal_deleter::operator()(internal* i) {
  delete i;
};

boole::~boole() = default;
void
boole::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
  internal_->parser.lua = lua_;
}
void
boole::eval(bool enable) {
  internal_->parser.eval = enable;
}
result
boole::operator()() {
  return generate_result(internal_->parser.formula()->o);
}
}
