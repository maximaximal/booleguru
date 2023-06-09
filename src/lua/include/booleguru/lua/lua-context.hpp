#pragma once

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

namespace booleguru::expression {
class op_manager;
class op_ref;
}

namespace sol {
class state;
}

namespace booleguru::lua {
class lua_context {
  std::shared_ptr<expression::op_manager> ops_;
  std::unique_ptr<sol::state> state_;
  std::string fennel_last_op_name_;

  void init_fennel();
  void register_booleguru_types();

#ifdef EMSCRIPTEN
  void register_js_require_cb();
#endif

  expression::op_ref get_var(const std::string& name);

  public:
  lua_context(std::shared_ptr<expression::op_manager> ops);
  lua_context();
  ~lua_context();

  using eval_result =
    std::variant<std::monostate, long int, std::string, expression::op_ref>;

  eval_result eval_fennel(std::string_view code);
  eval_result eval_fennel(std::string_view code,
                          const expression::op_ref& last_op);

  eval_result eval(std::string_view code);
  eval_result eval(std::string_view code, const expression::op_ref& last_op);
};
}
