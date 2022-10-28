#include <booleguru/parse/base.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
base::base(std::istream& in,
           std::shared_ptr<expression::var_manager> vars,
           std::shared_ptr<expression::script_manager> scripts,
           std::shared_ptr<expression::op_manager> ops)
  : in_(in)
  , vars_(vars)
  , scripts_(scripts)
  , ops_(ops) {}

base::base(std::istream& in, base& b)
  : in_(in)
  , vars_(b.vars_)
  , scripts_(b.scripts_)
  , ops_(b.ops_) {}

base::~base() {}

result
base::generate_result(expression::op_ref expr) {
  return result{ .expr = expr,
                 .vars = vars_,
                 .scripts = scripts_,
                 .ops = ops_,
                 .line = line_,
                 .column = column_ };
}

result
base::error(std::string_view error) {
  return result{ .vars = vars_,
                 .scripts = scripts_,
                 .ops = ops_,
                 .line = line_,
                 .column = column_,
                 .message{ std::string(error) } };
}
}
