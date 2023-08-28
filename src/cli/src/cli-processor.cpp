#include <booleguru/cli/argument.hpp>
#include <booleguru/cli/cli-processor.hpp>
#include <booleguru/cli/input_file.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>

#include <booleguru/util/istringviewstream.hpp>

#include <booleguru/parse/cli.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>

namespace booleguru::cli {

cli_processor::cli_processor(int argc,
                             const char* argv[],
                             std::shared_ptr<expression::op_manager> ops,
                             std::shared_ptr<lua::lua_context> lua)
  : begin_(argv)
  , end_(argv + argc)
  , ops_(ops ? ops : std::make_shared<expression::op_manager>())
  , lua_(lua ? lua : std::make_shared<lua::lua_context>(ops_)) {
  // Register this to be the global handler!
  expression::literals::handle::global(ops_);
}

expression::op_ref
cli_processor::process() {
  auto cmd = fmt::memory_buffer();
  fmt::format_to(
    std::back_inserter(cmd), "{}", fmt::join(begin_ + 1, end_, " "));
  std::string_view cmd_view(cmd.data(), cmd.size());
  isviewstream cmd_istream(cmd_view);
  parse::cli cli(cmd_istream, ops_->vars_ptr(), ops_, lua_);

  cli.parse_file_using([this](std::string_view path) {
    input_file in(path, ops_, lua_);
    return in.process();
  });

  parse::result result = cli();
  output_type_ = cli.output_type();
  if(!result) {
    throw cli_parse_error(result.message);
  }
  return *result;
};
}
