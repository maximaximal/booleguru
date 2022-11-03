#include <ecl/ecl.h>

#include <booleguru/cl/ecl-wrapper.hpp>

extern "C" {
#include <init-wrapper.h>
extern void ECL_INIT_LIB_FUNC(cl_object);
}

namespace booleguru::cl {
// https://stackoverflow.com/a/41934969
std::string
ecl_string_to_string(cl_object echar) {
  switch(ecl_t_of(echar)) {
#ifdef ECL_UNICODE
    case t_string:
      if(!ecl_fits_in_base_string(echar)) {
        echar = cl_copy_seq(echar);
      } else {
        echar = si_copy_to_simple_base_string(echar);
      }
      break;
#endif
    case t_base_string:
      // OK
      break;
    default:
      // PRINT SOME ERROR
      return std::string();// or raise an exception
  }

  std::string res("");
  int j = echar->base_string.dim;               // get dimension
  ecl_base_char* selv = echar->base_string.self;// get pointer

  // do simple pointer addition
  for(int i = 0; i < j; i++) {
    res += (*(selv + i));
  }
  return res;
};

std::unique_ptr<ecl_wrapper, ecl_wrapper::deleter> ecl_wrapper::wrapper_;

cl_object clfun_eval;

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);

  clfun_eval = cl_eval(c_string_to_object("#'eval-sexp-and-catch-errors"));
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

  cl_object ret;

  cl_env_ptr env = ecl_process_env();
  ECL_CATCH_ALL_BEGIN(env) {

    if(interactive_debugger_) {
      ret = cl_eval(form);
    } else {
      ret = cl_funcall(2, clfun_eval, form);
      if(ecl_t_of(ret) == t_fixnum) {
        if(ecl_fixnum(ret) == -1) {
          return "Safe Eval returned -1!";
        }
      } else if(ECL_EXTENDED_STRING_P(ret)) {
        return ecl_string_to_string(ret);
      }
    }
  }
  ECL_CATCH_ALL_IF_CAUGHT {
    return "Caught error!";
  }
  ECL_CATCH_ALL_END;

  if(ecl_t_of(ret) == t_fixnum) {
    return static_cast<long int>(ecl_fixnum(ret));
  }
  return std::monostate();
}
}
