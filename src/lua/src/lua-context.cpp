#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <booleguru/expression/op_manager.hpp>

#ifdef BOOLEGURU_LUA_AUTOSTART_DIR
static void
prepare_state(sol::state& s) {
  s.script_file(BOOLEGURU_LUA_AUTOSTART_DIR "/graph-printer.lua");
  s.require_file("fennel", BOOLEGURU_LUA_AUTOSTART_DIR "/fennel.lua");
}
#else
extern const char fennel_lua[];
extern const unsigned fennel_lua_size;
static const std::string_view fennel_lua_string_view(fennel_lua,
                                                     fennel_lua_size);

extern const char graph_printer_lua[];
extern const unsigned graph_printer_lua_size;
static const std::string_view graph_printer_lua_string_view(
  graph_printer_lua,
  graph_printer_lua_size);

static void
prepare_state(sol::state& s) {
  s.script(graph_printer_lua_string_view);
  s.require_script("fennel", fennel_lua_string_view);
}
#endif

namespace booleguru::lua {
lua_context::lua_context(std::shared_ptr<expression::op_manager> ops)
  : ops_(ops)
  , state_(std::make_unique<sol::state>()) {
  state_->open_libraries(sol::lib::base,
                         sol::lib::coroutine,
                         sol::lib::package,
                         sol::lib::string,
                         sol::lib::table,
                         sol::lib::math,
                         sol::lib::io);

  // Later, this could be handled as autoloads, as described here:
  // https://www.lua.org/pil/15.5.html
  prepare_state(*state_);

  init_fennel();

  register_booleguru_types();
}

lua_context::lua_context()
  : lua_context(std::make_shared<expression::op_manager>()) {}

lua_context::~lua_context() {}

void
lua_context::init_fennel() {
  state_->script("fennel = require(\"fennel\")");
  fennel_last_op_name_ = (*state_)["fennel"]["mangle"]("*last-op*");
}

static lua_context::eval_result
return_to_eval_result(auto&& result) {
  if(!result.valid()) {
    sol::error err = result;
    std::string what = err.what();
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

lua_context::eval_result
lua_context::eval_fennel(std::string_view code) {
  auto eval = (*state_)["fennel"]["eval"];
  auto r = eval(code);
  return return_to_eval_result(r);
}

lua_context::eval_result
lua_context::eval_fennel(std::string_view code,
                         const expression::op_ref& last_op) {
  (*state_)[fennel_last_op_name_] = last_op;
  return eval_fennel(code);
}

lua_context::eval_result
lua_context::eval(std::string_view code) {
  auto r = state_->script(code);
  return return_to_eval_result(r);
}

lua_context::eval_result
lua_context::eval(std::string_view code, const expression::op_ref& last_op) {
  (*state_)["last_op"] = last_op;
  return eval(code);
}

}
