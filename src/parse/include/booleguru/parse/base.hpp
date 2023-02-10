#pragma once

#include <istream>
#include <memory>

namespace booleguru::expression {
enum class op_type;
class op_ref;
class var_manager;
class op_manager;
}

namespace booleguru::lua {
class lua_context;
}

namespace booleguru::parse {
struct result;

class base {
  protected:
  std::shared_ptr<expression::var_manager> vars_;
  std::shared_ptr<expression::op_manager> ops_;
  std::shared_ptr<lua::lua_context> lua_;
  std::istream& in_;

  int line_ = 0;
  int column_ = 0;

  result generate_result(expression::op_ref expr);
  result error(std::string_view error, int code = 0);

  public:
  base(std::istream& in,
       std::shared_ptr<expression::var_manager> vars,
       std::shared_ptr<expression::op_manager> ops,
       std::shared_ptr<lua::lua_context> lua_context);
  base(std::istream& in, std::shared_ptr<expression::op_manager> ops);

  base(std::istream& in, base& b);

  base(std::istream& in);

  virtual ~base();

  virtual result operator()() = 0;
};
}
