#include <ecl/ecl.h>

#include <booleguru/cl/ecl-wrapper.hpp>

#define ECL_INIT_LIB_FUNC init_lib__ECL24K3EGG5RZFPZ_VNORAE61

extern "C" {
extern void init_lib__BOOLEGURU_CL(cl_object);
extern void ECL_INIT_LIB_FUNC(cl_object);
}


namespace booleguru::cl {
std::unique_ptr<ecl_wrapper, ecl_wrapper::deleter> ecl_wrapper::wrapper_;

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);

  cl_eval(c_string_to_object("(hello-world)"));
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
}
