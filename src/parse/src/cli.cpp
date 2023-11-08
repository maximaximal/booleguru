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

#include <fmt/format.h>

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
class cli_graph : public expression::op_graph {
  lua::lua_context& lua;
  cli::parse_file_function f;

  public:
  cli_graph(expression::op_manager& ops,
            lua::lua_context& lua,
            cli::parse_file_function f)
    : op_graph(ops)
    , lua(lua)
    , f(f) {}

  virtual ref fennel_(std::string_view code, ref last_op = 0) final override {
    expression::op_ref op = ops[last_op];
    return lua.eval_fennel_to_op_or_throw(code, op).get_id();
  }
  virtual ref file_(std::string_view path, util::type type) final override {
    assert(f);
    return f(path, type).get_id();
  }
};

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
}
result
cli::operator()() {
  if(!internal_) {
    init();
  }

  cli_graph g(*ops_, *lua_, parse_file_function_);

  internal_->parser.g = &g;

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

size_t
cli::fuzz_files() {
  if(!internal_) {
    init();
  }

  expression::fuzz_file_graph f;

  internal_->parser.g = &f;

  try {
    internal_->parser.invocation();
  } catch(parser_exception& e) {
  }

  return f.number_of_fuzz_files();
}

}
