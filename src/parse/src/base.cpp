#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

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

base::base(std::istream& in)
  : in_(in)
  , vars_(std::make_shared<expression::var_manager>())
  , scripts_(std::make_shared<expression::script_manager>())
  , ops_(std::make_shared<expression::op_manager>(vars_, scripts_)) {}

base::~base() {}

result
base::generate_result(expression::op_ref expr) {
  return result{ .expr = expr, .line = line_, .column = column_ };
}

result
base::error(std::string_view error) {
  return result{ .line = line_,
                 .column = column_,
                 .message{ std::string(error) } };
}
}
