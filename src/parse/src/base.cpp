#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/parse/base.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
base::base(std::istream& in,
           std::shared_ptr<expression::var_manager> vars,

           std::shared_ptr<expression::op_manager> ops)
  : vars_(vars)
  , ops_(ops)
  , in_(in) {}

base::base(std::istream& in, base& b)
  : vars_(b.vars_)
  , ops_(b.ops_)
  , in_(in) {}

base::base(std::istream& in)
  : vars_(std::make_shared<expression::var_manager>())
  , ops_(std::make_shared<expression::op_manager>(vars_))
  , in_(in) {}

base::base(std::istream& in, std::shared_ptr<expression::op_manager> ops)
  : vars_(ops->vars_ptr())
  , ops_(ops)
  , in_(in) {}

base::~base() {}

result
base::generate_result(expression::op_ref expr) {
  return result{ .expr = expr, .line = line_, .column = column_, .message = "", .code = result::error_code::OTHER };
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
