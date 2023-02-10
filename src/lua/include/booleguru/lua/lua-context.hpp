#pragma once

#include <memory>
#include <optional>
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

  bool fennel_initiated_ = false;
  void init_fennel();

  public:
  lua_context(std::shared_ptr<expression::op_manager> ops);
  lua_context();
  ~lua_context();

  using eval_result =
    std::variant<std::monostate, long int, std::string, expression::op_ref>;

  eval_result eval_fennel(std::string_view code,
                          std::optional<uint32_t> last_op = std::nullopt);
};
}
