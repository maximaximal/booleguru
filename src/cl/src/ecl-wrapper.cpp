#include <ecl/ecl.h>

#include <booleguru/cl/ecl-wrapper.hpp>

extern "C" {
#include <init-wrapper.h>
extern void ECL_INIT_LIB_FUNC(cl_object);
}

namespace booleguru::cl {
std::unique_ptr<ecl_wrapper, ecl_wrapper::deleter> ecl_wrapper::wrapper_;

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);
}
ecl_wrapper::~ecl_wrapper() {
  cl_shutdown();
}

ecl_wrapper&
ecl_wrapper::get() {
  if(!wrapper_)
    wrapper_.reset(new ecl_wrapper());
  return *wrapper_;
}

ecl_wrapper::supported_return_types
ecl_wrapper::eval(const char* code) {
  cl_object form = c_string_to_object(code);
  cl_object ret = cl_eval(form);

  if(ecl_t_of(ret) == t_fixnum) {
    return fix(ret);
  }
  return std::monostate();
}
}
