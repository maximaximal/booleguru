// Required because of linking issues with ECL transitively including GMP
#include <gmp.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-register"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wregister"
extern "C" {
#include <ecl/ecl.h>
}
#pragma clang diagnostic pop
#pragma GCC diagnostic pop


#include <booleguru/expression/op_manager.hpp>

#include <booleguru/cl/cl-function.hpp>
#include <booleguru/cl/cl-globals.hpp>

namespace booleguru::cl {
using namespace expression;

std::optional<cl_object>
cl_object_conv(const cl_object& src, op_ref& tgt) {
  uint32_t id;

  if(ECL_FIXNUMP(src)) {
    id = ecl_to_uint32_t(src);
  } else if(ECL_INSTANCEP(src) && ECL_STRUCT_NAME(src) == cltype_op) {
    id = ecl_to_uint32_t(ECL_STRUCT_SLOT(src, 0));
  } else {
    return make_type_error("'unknown", "'op");
  }

  tgt = (*op_manager)[id];
  return std::nullopt;
}

cl_object
make_type_error(std::string datum, std::string expected) {
  std::string cl =
    "(error 'type-error :datum " + datum + " :expected-type " + expected + ")";
  cl_object form = c_string_to_object(cl.c_str());
  return cl_eval(form);
}
}
