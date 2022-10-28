#pragma once

#include <istream>
#include <memory>

namespace booleguru::expression {
class op_ref;
class var_manager;
class op_manager;
class script_manager;
}

namespace booleguru::parse {
class result;

class base {
  protected:
  std::shared_ptr<expression::var_manager> vars_;
  std::shared_ptr<expression::script_manager> scripts_;
  std::shared_ptr<expression::op_manager> ops_;
  std::istream& in_;

  int line_ = 0;
  int column_ = 0;

  result generate_result(expression::op_ref expr);
  result error(std::string_view error);

  public:
  base(std::istream& in,
       std::shared_ptr<expression::var_manager> vars,
       std::shared_ptr<expression::script_manager> scripts,
       std::shared_ptr<expression::op_manager> ops);

  base(std::istream& in, base& b);

  virtual ~base();

  virtual result operator()() = 0;
};
}
