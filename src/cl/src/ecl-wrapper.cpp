#include <ecl/ecl.h>

#include <booleguru/cl/ecl-wrapper.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <iostream>

// Interesting: https://lambdafaktorie.com/embedding-lisp-in-c-a-recipe/
//
// Another good example: https://gist.github.com/vwood/662109

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

static cl_object clfun_eval;
static thread_local booleguru::expression::op_manager* op_manager = nullptr;

#define DEFUN(name, fun, args) \
  ecl_def_c_function(c_string_to_object(name), (cl_objectfn_fixed)fun, args)

cl_object
clfun_var(cl_object name) {
  // The symbol: std::cout << ecl_string_to_string(cl_symbol_name(name)) <<
  // std::endl;
  if(ECL_SYMBOLP(name)) {
    name = cl_symbol_name(name);
  }

  std::string varname = ecl_string_to_string(name);
  uint32_t varid =
    op_manager->vars().get(expression::variable{ varname }).get_id();
  return ecl_make_uint32_t(varid);
}

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);

  clfun_eval = cl_eval(c_string_to_object("#'eval-sexp-and-catch-errors"));

  DEFUN("var", clfun_var, 1);
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
ecl_wrapper::eval(const char* code,
                  std::shared_ptr<expression::op_manager> ops) {
  cl_object form = c_string_to_object(code);

  cl_object ret;

  if(ops)
    op_manager = ops.get();

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
