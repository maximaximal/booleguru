#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <booleguru/expression/op_manager.hpp>

extern const char fennel_lua[];
extern const unsigned fennel_lua_size;
static const std::string_view fennel_lua_string_view(fennel_lua,
                                                     fennel_lua_size);

namespace booleguru::lua {
lua_context::lua_context(std::shared_ptr<expression::op_manager> ops)
  : ops_(ops)
  , state_(std::make_unique<sol::state>()) {
  state_->open_libraries(sol::lib::base,
                         sol::lib::coroutine,
                         sol::lib::package,
                         sol::lib::string,
                         sol::lib::table,
                         sol::lib::math);

  state_->require_script("fennel", fennel_lua_string_view);

  init_fennel();

  register_booleguru_types();
}

lua_context::lua_context()
  : lua_context(std::make_shared<expression::op_manager>()) {}

lua_context::~lua_context() {}

void
lua_context::init_fennel() {
  state_->script("fennel = require(\"fennel\")");
}

lua_context::eval_result
lua_context::eval_fennel(std::string_view code,
                         std::optional<uint32_t> last_op) {
  (void)code;
  (void)last_op;

  auto eval = (*state_)["fennel"]["eval"];
  auto result = eval(code);

  if(!result.valid()) {
    sol::error err = result;
    std::string what = err.what();
    std::cout << what << std::endl;
    return what;
  }

  if(result.get_type() == sol::type::number) {
    return static_cast<long int>(result);
  }

  if(result.get_type() == sol::type::string) {
    return static_cast<std::string>(result);
  }

  if(result.get_type() == sol::type::userdata) {
    return static_cast<expression::op_ref>(result);
  }

  return std::monostate{};
}
}
