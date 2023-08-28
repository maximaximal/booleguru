#include <booleguru/parse/cli.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/expression/var_manager.hpp>
#include <booleguru/lua/lua-context.hpp>

#include <exception>
#include <iostream>
#include <sstream>

#include <cli_lexer.h>
#include <cli_parser.h>

#include "parser-internal.hpp"

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
struct cli::internal
  : public parser_internal<antlr::cli_lexer, antlr::cli_parser> {
  using base = parser_internal<cli_lexer, cli_parser>;
  using base::parser_internal;

  virtual ~internal() {}
};
cli::~cli() = default;
void
cli::internal_deleter::operator()(cli::internal* i) {
  delete i;
}
void
cli::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
  internal_->parser.lua = lua_;
}
result
cli::operator()() {
  if(!internal_) {
    init();
  }

  internal_->parser.parse_file_function_ = parse_file_function_;

  try {
    auto result = internal_->parser.invocation();
    output_type_ = internal_->parser.out_type;
    return generate_result((*ops_)[result->o]);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
}
}
