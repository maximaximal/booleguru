#include <ecl/ecl.h>

#include <booleguru/expression/op_manager.hpp>

#include <booleguru/cl/cl-function.hpp>
#include <booleguru/cl/cl-globals.hpp>

namespace booleguru::cl {
std::optional<cl_object>
cl_object_conv(const cl_object& src, expression::op_ref& tgt) {
  if(!ECL_INSTANCEP(src) || ECL_STRUCT_NAME(src) != cltype_op) {
    return make_error("not-an-op");
  }

  uint32_t id = ecl_to_uint32_t(ECL_STRUCT_SLOT(src, 0));
  tgt = (*op_manager)[id];
  return std::nullopt;
}

cl_object
make_error(std::string symbol, std::map<std::string, std::string> properties) {
  std::string cl = "(error '" + symbol;
  for(const auto& p : properties) {
    cl += " :" + p.first + " " + p.second;
  }
  cl += ")";
  cl_object form = c_string_to_object(cl.c_str());
  return cl_eval(form);
}
}
