#include <iostream>

#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>

#include <booleguru/expression/var_manager.hpp>

#include <smtlib2_lexer.h>
#include <smtlib2_parser.h>

#include "parser-internal.hpp"

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
struct smtlib2::internal : public parser_internal<smtlib2_lexer, smtlib2_parser> {
  using parser_internal<smtlib2_lexer, smtlib2_parser>::parser_internal;
};

void
smtlib2::internal_deleter::operator()(smtlib2::internal* i) {
  delete i;
}

void
smtlib2::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
  internal_->parser.bvops = std::make_shared<expression::bvop_manager>();
}

smtlib2::~smtlib2() = default;

result
smtlib2::operator()() {
  if(internal_ == nullptr)
    init();
  // Invoke Antlr runtime
  try {
    internal_->parser.formula();
    return generate_result(internal_->parser.assertions);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
  return {};
}
}
