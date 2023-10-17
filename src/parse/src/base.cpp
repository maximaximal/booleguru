#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/parse/base.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/lua/lua-context.hpp>

namespace booleguru::parse {
base::base(std::istream& in,
           std::shared_ptr<expression::var_manager> vars,
           std::shared_ptr<expression::op_manager> ops,
           std::shared_ptr<lua::lua_context> lua)
  : vars_(vars)
  , ops_(ops)
  , lua_(lua)
  , in_(in) {
  init();
}

base::base(std::istream& in, base& b)
  : vars_(b.vars_)
  , ops_(b.ops_)
  , lua_(b.lua_)
  , in_(in) {
  init();
}

base::base(std::istream& in)
  : vars_(std::make_shared<expression::var_manager>())
  , ops_(std::make_shared<expression::op_manager>(vars_))
  , lua_(std::make_shared<lua::lua_context>(ops_))
  , in_(in) {
  init();
}

base::base(std::istream& in, std::shared_ptr<expression::op_manager> ops)
  : vars_(ops->vars_ptr())
  , ops_(ops)
  , lua_(std::make_shared<lua::lua_context>(ops_))
  , in_(in) {
  init();
}

base::~base() {}

result
base::generate_result(expression::op_ref expr) {
  return result{ .expr = expr,
                 .line = line_,
                 .column = column_,
                 .message = "",
                 .code = result::error_code::OTHER };
}

result
base::error(std::string_view error, int code) {
  return result{ .expr = expression::op_ref(),
                 .line = line_,
                 .column = column_,
                 .message{
                   std::string(error),
                 },
                 .code = static_cast<result::error_code>(code) };
}
}
