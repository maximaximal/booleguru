#include <cstdlib>
#include <sol/error.hpp>
#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

#include <fmt/format.h>

#include <booleguru/lua/lua-context.hpp>
#include <booleguru/util/str_replace.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>

#ifdef EMSCRIPTEN
using more_data_cb = std::function<std::string_view(std::string)>;
extern more_data_cb js_more_data_cb;
#endif

#ifdef BOOLEGURU_LUA_AUTOSTART_DIR
static void
prepare_state(sol::state& s) {
  const std::string package_path = s["package"]["path"];
  s["package"]["path"] = package_path + (!package_path.empty() ? ";" : "")
                         + std::string(BOOLEGURU_LUA_AUTOSTART_DIR) + "/?.lua";
}
static void
prepare_state_after_fennel_install(sol::state& s) {
  const std::string fennel_path = s["fennel"]["path"];
  s["fennel"]["path"] = fennel_path + (!fennel_path.empty() ? ";" : "")
                        + std::string(BOOLEGURU_LUA_AUTOSTART_DIR) + "/?.fnl";
}
#else
#include <scripts.h>

static void
prepare_state(sol::state& s) {
  for(const embedded_script& e : embedded_scripts) {
    std::string_view name = e.name;
    const std::string_view& data = e.data;

    if(name.ends_with("_lua")) {
      name.remove_suffix(4);
      std::string n = std::string(name);
      s.require_script(n, data);
      if(n.find("_") != std::string::npos) {
	// Return the same script as above using an additional
	// require. It will be resolved dynamically if needed, but the
	// Lua state does not have to duplicate all of the scripts
	// from before.
        booleguru::util::str_replace(n, "_", "-");
        s.require_script(
          n, std::string("return require(\"") + std::string(name) + "\")");
      }
      continue;
    }
  }
}
static void
prepare_state_after_fennel_install(sol::state& s) {
  for(const embedded_script& e : embedded_scripts) {
    std::string_view name = e.name;
    const std::string_view& data = e.data;

    if(name.ends_with("_fnl")) {
      name.remove_suffix(4);
      s.require_script(
        std::string(name),
        static_cast<std::string>(s["fennel"]["compileString"](data)));
      continue;
    }
  }
}
#endif

#if defined(WIN32) || defined(_WIN32)
#define SEP ';'
#else
#define SEP ':'
#endif

namespace booleguru::lua {
lua_context::lua_context(std::shared_ptr<expression::op_manager> ops)
  : ops_(ops)
  , state_(std::make_unique<sol::state>()) {}

lua_context::~lua_context() {}

void
lua_context::ensure_fully_initialized() {
  if(fully_initialized_)
    return;
  fully_initialized_ = true;

  state_->open_libraries(sol::lib::base,
                         sol::lib::coroutine,
                         sol::lib::package,
                         sol::lib::string,
                         sol::lib::table,
                         sol::lib::math,
                         sol::lib::io,
                         sol::lib::debug);
  // Later, this could be handled as autoloads, as described here:
  // https://www.lua.org/pil/15.5.html
  prepare_state(*state_);

  init_fennel();

  prepare_state_after_fennel_install(*state_);

  register_booleguru_types();

  if(const char* lua_path_env = std::getenv("BOOLEGURU_LUA_PATH")) {
    auto& s = *state_;
    std::stringstream lua_path_env_stringstream(lua_path_env);

    std::string lua_package_path = s["package"]["path"];

    std::string p;
    while(std::getline(lua_path_env_stringstream, p, SEP)) {
      lua_package_path += (!lua_package_path.empty() ? ";" : "") + p + "/?.lua";
    }
    s["package"]["path"] = lua_package_path;
  }

  if(const char* lua_fennel_env = std::getenv("BOOLEGURU_FENNEL_PATH")) {
    auto& s = *state_;
    std::stringstream lua_path_env_stringstream(lua_fennel_env);

    std::string fennel_path = s["fennel"]["path"];

    std::string p;
    while(std::getline(lua_path_env_stringstream, p, SEP)) {
      fennel_path += (!fennel_path.empty() ? ";" : "") + p + "/?.fnl";
    }
    s["fennel"]["path"] = fennel_path;
  }

#ifdef EMSCRIPTEN
  register_js_require_cb();
#endif
}

#undef SEP

void
lua_context::init_fennel() {
  state_->script("fennel = require(\"fennel\").install()");
  auto mangler = (*state_)["fennel"]["mangle"];

  fennel_last_op_name_ = mangler("**");
  fennel_l_name_ = mangler("*l*");
  fennel_r_name_ = mangler("*r*");
}

#ifdef EMSCRIPTEN
std::string_view js_more_data_searcher =
  R"(package.searchers[#package.searchers + 1] = function(libraryname)
  r = js_more_data_cb(libraryname)
  if r == "" then
    return nil
  end
  return r
end
)";

void
lua_context::register_js_require_cb() {
  auto& s = *state_;
  s["js_more_data_cb"] = js_more_data_cb;
  s.script(js_more_data_searcher);
}
#endif

static lua_context::eval_result
return_to_eval_result(auto&& result) {
  if(!result.valid()) {
    sol::error err = result;
    std::string what = err.what();
    return what;
  }

  if(result.get_type() == sol::type::number) {
    try {
      return static_cast<long int>(result);
    } catch(sol::error& e) {
      return std::string("script did not return an integer!");
    }
  }

  if(result.get_type() == sol::type::string) {
    return static_cast<std::string>(result);
  }

  if(result.get_type() == sol::type::userdata) {
    return static_cast<expression::op_ref>(result);
  }

  return std::monostate{};
}

std::string
lua_context::fennel_mangle(const std::string& name) {
  return (*state_)["fennel"]["mangle"](name);
}

lua_context::eval_result
lua_context::eval_fennel(std::string_view code) {
  ensure_fully_initialized();
  auto& s = *state_;

  auto eval = s["fennel"]["eval"];

  // Try to auto-load the desired function if it is not known yet.
  if(code.starts_with("(")) {
    std::string_view fun = code;
    fun.remove_prefix(1);
    while(fun.starts_with(" ")) {
      fun.remove_prefix(1);
    }
    size_t fun_end = fun.find_first_of(" ");
    if(fun_end == std::string_view::npos) {
      fun_end = fun.find_first_of("\"");
      if(fun_end == std::string_view::npos) {
        fun_end = fun.find_first_of(".");
        if(fun_end == std::string_view::npos) {
          fun_end = fun.find_first_of("(");
          if(fun_end == std::string_view::npos) {
            fun_end = fun.find_first_of(")");
          }
        }
      }
    }
    fun.remove_suffix(fun.length() - fun_end);

    std::string fun_str(fun);

    // There are some special forms in Fennel that should not be
    // loaded. We ignore them explicitly here.

    if(fun_str != "+" && fun_str != "-" && !s[fennel_mangle(fun_str)].valid()) {
      using namespace std::literals::string_literals;
      std::string call = "(let ["s + fun_str + " (require :" + fun_str + ")]\n"
                         + std::string(code) + ")";
      auto r = eval(call);
      return return_to_eval_result(r);
    }
  }

  auto r = eval(code);
  return return_to_eval_result(r);
}

lua_context::eval_result
lua_context::eval_fennel(std::string_view code,
                         const expression::op_ref& last_op) {
  ensure_fully_initialized();
  (*state_)[fennel_last_op_name_] = last_op;
  return eval_fennel(code);
}

lua_context::eval_result
lua_context::eval(std::string_view code) {
  ensure_fully_initialized();
  auto r = state_->script(code);
  return return_to_eval_result(r);
}

lua_context::eval_result
lua_context::eval(std::string_view code, const expression::op_ref& last_op) {
  ensure_fully_initialized();
  (*state_)["**"] = last_op;
  return eval(code);
}

lua_context::eval_result
lua_context::eval_fennel(std::string_view code,
                         const expression::op_ref& l,
                         const expression::op_ref& r) {
  ensure_fully_initialized();
  (*state_)[fennel_l_name_] = l;
  (*state_)["left_op"] = l;
  (*state_)[fennel_r_name_] = r;
  (*state_)["right_op"] = r;
  return eval_fennel(code);
}

expression::op_ref
lua_context::eval_fennel_to_op_or_throw(std::string_view code,
                                        expression::op_ref last_op) {
  ensure_fully_initialized();
  auto res = eval_fennel(code, last_op);
  if(std::string* s = std::get_if<std::string>(&res)) {
    throw fennel_error(*s);
  }
  if(expression::op_ref* op = std::get_if<expression::op_ref>(&res)) {
    return *op;
  }
  return expression::op_ref();
}

expression::op_ref
lua_context::eval_fennel_to_op_or_throw(std::string_view code,
                                        expression::op_ref l,
                                        expression::op_ref r) {
  ensure_fully_initialized();
  auto res = eval_fennel(code, l, r);
  if(std::string* s = std::get_if<std::string>(&res)) {
    throw fennel_error(*s);
  }
  if(expression::op_ref* op = std::get_if<expression::op_ref>(&res)) {
    return *op;
  }
  return expression::op_ref();
}

expression::op_ref
lua_context::eval_fennel_to_op_or_throw(std::string_view code) {
  return eval_fennel_to_op_or_throw(code, expression::op_ref());
}

}
