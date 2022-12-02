#include <ecl/ecl.h>

#include <booleguru/cl/cl-function.hpp>
#include <booleguru/cl/cl-globals.hpp>
#include <booleguru/cl/ecl-wrapper.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/transform/cnf.hpp>
#include <booleguru/transform/distribute_nots.hpp>
#include <booleguru/transform/distribute_ors.hpp>
#include <booleguru/transform/variable_extend.hpp>
#include <booleguru/transform/variable_rename.hpp>
#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/eliminate_implication.hpp>

#include <iostream>

// Interesting: https://lambdafaktorie.com/embedding-lisp-in-c-a-recipe/
//
// Another good example: https://gist.github.com/vwood/662109
// Another one: https://github.com/earl-ducaine/stupid-ecl-tricks-1

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

#define DEFUN(name, fun, args) \
  ecl_def_c_function(c_string_to_object(name), (cl_objectfn_fixed)fun, args)

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);

  cl_eval(c_string_to_object("(in-package cl-user)"));

  clfun_eval = cl_eval(c_string_to_object("#'eval-sexp-and-catch-errors"));
  clfun_b_define_global_last_op =
    cl_eval(c_string_to_object("#'b-define-global-last-op"));
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
                  std::shared_ptr<expression::op_manager> ops,
                  std::optional<uint32_t> last_op) {
  if(ops)
    op_manager = ops;

  cl_object form = c_string_to_object(code);

  if(last_op)
    cl_funcall(2, clfun_b_define_global_last_op, ecl_make_uint32_t(*last_op));

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

  if(ECL_FIXNUMP(ret)) {
    int id = ecl_to_uint32_t(ret);
    return (*op_manager)[id];
  }
  if(ECL_INSTANCEP(ret)) {
    cl_object structname = ECL_STRUCT_NAME(ret);
    if(structname == cltype_op) {
      int id = ecl_fixnum(ECL_STRUCT_SLOT(ret, 0));
      return (*op_manager)[id];
    }
  }
  return std::monostate();
}
}
