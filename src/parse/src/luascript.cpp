#include <booleguru/lua/lua-context.hpp>

#include <booleguru/parse/luascript.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
result
luascript::operator()() {
  std::string code(std::istreambuf_iterator<char>(in_), {});
  auto eval_result = lua_->eval(code);
  if(std::string* err = std::get_if<std::string>(&eval_result)) {
    return error(*err);
  } else if(expression::op_ref* op
            = std::get_if<expression::op_ref>(&eval_result)) {
    return generate_result(*op);
  } else {
    return error("Invalid return type of lua script! Has to return an op.");
  }
}
}
