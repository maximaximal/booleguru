#include <cassert>
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
          str == "boole" || str == "limboole") {
    param = str;
    return argument::type;
  }

  return argument::unknown;
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
      if(param == "boole" || param == "limboole")
        return argument::boole;
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
  : argument(arg.find('=') ? arg.substr(0, arg.find('=')) : arg,
             arg.find('=') ? arg.substr(arg.find('=')) : "") {}
}