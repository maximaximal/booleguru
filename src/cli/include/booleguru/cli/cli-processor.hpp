#pragma once

#include <memory>
#include <string_view>
#include <vector>

#include <booleguru/parse/type.hpp>

namespace booleguru::expression {
class op_ref;
class op_manager;
}

namespace booleguru::lua {
class lua_context;
}

struct no_input_file : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct cli_parse_error : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct fennel_error : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct fennel_invalid_return_type : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

namespace booleguru::cli {
class cli_processor {
  private:
  parse::type output_type_ = parse::type::boole;
  const char** begin_;
  const char** end_;

  public:
  using arg_vec = std::vector<std::string_view>;

  explicit cli_processor(int argc,
                         const char* argv[],
                         std::shared_ptr<expression::op_manager> ops = nullptr,
                         std::shared_ptr<lua::lua_context> lua = nullptr);

  expression::op_ref process();

  parse::type output_type() const { return output_type_; };

  void ensure_lua_initialized();

  std::shared_ptr<expression::op_manager> ops_;
  std::shared_ptr<lua::lua_context> lua_;

  void next();
};
}
