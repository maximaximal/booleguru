#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <exception>
#include <iostream>
#include <sstream>

#include <boole_lexer.h>
#include <boole_parser.h>

#include "parser-internal.hpp"

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
struct boole::internal
  : public parser_internal<antlr::boole_lexer, antlr::boole_parser> {
  using base = parser_internal<boole_lexer, boole_parser>;
  using base::parser_internal;

  virtual ~internal() {}
};
boole::~boole() = default;
void
boole::internal_deleter::operator()(boole::internal* i) {
  delete i;
}
void
boole::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
  internal_->parser.lua = lua_;
}
void
boole::eval(bool enable) {
  if(!internal_) {
    init();
  }
  internal_->parser.eval = enable;
}
result
boole::operator()() {
  if(!internal_) {
    init();
  }
  try {
    auto result = internal_->parser.formula();
    return generate_result(result->o);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
}
}
