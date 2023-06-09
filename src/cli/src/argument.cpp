#include <cassert>
#include <sstream>
#include <string_view>
#include <unordered_map>

#include <booleguru/cli/argument.hpp>

namespace booleguru::cli {
static argument::keywords
keyword_from_string_view(std::string_view str, std::string_view& param) {
  if(str == "eval")
    return argument::eval;
  else if(str == "type")
    return argument::type;
  else if(str == "qcir" || str == "smtlib2" || str == "smtlib" ||
          str == "smt" || str == "dimacs" || str == "qdimacs" ||
          str == "boole" || str == "limboole" || str == "lua" || str == "py" ||
          str == "python" || str == "null") {
    param = str;
    return argument::type;
  } else if(str == "ns" || str == "namespace") {
    return argument::variable_namespace;
  }

  std::stringstream err;
  err << "unknown argument: " << str;
  throw unknown_argument(err.str());
}

static argument::param_variant
variant_from_param(argument::keywords k, std::string_view param) {
  argument::param_variant v;
  switch(k) {
    case argument::eval:
      if(param == "true" || param == "1" || param == "yes" || param == "y" ||
         param == "t" || param == "on" || param == "")
        return true;
      return false;
    case argument::type:
      if(param == "qcir")
        return argument::qcir;
      if(param == "smtlib2" || param == "smtlib" || param == "smt")
        return argument::smtlib2;
      if(param == "dimacs" || param == "qdimacs")
        return argument::qdimacs;
      if(param == "python" || param == "py")
        return argument::py;
      if(param == "lua")
        return argument::lua;
      if(param == "boole" || param == "limboole")
        return argument::boole;
      if(param == "none" || param == "no" || param == "off" || param == "null")
        return argument::none;
    case argument::variable_namespace:
      return param;
    default:
      break;
  }
  return v;
}

argument::argument(std::string_view arg, std::string_view param)
  : arg(arg)
  , keyword(keyword_from_string_view(arg, param))
  , param(variant_from_param(keyword, param)) {
  assert(arg.find('=') == std::string_view::npos);
}
argument::argument(std::string_view arg)
  : argument(
      arg.find('=') != std::string_view::npos ? arg.substr(0, arg.find('='))
                                              : arg,
      arg.find('=') != std::string_view::npos ? arg.substr(arg.find('=') + 1)
                                              : "") {}
}
