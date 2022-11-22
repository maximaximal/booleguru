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

cl_object
clfun_get_varop_id(cl_object name) {
  // The symbol: std::cout << ecl_string_to_string(cl_symbol_name(name)) <<
  // std::endl;
  if(ECL_SYMBOLP(name)) {
    name = cl_symbol_name(name);
  }

  std::string varname = ecl_string_to_string(name);
  uint32_t varid =
    op_manager->vars().get(expression::variable{ varname }).get_id();

  auto ref =
    op_manager->get(expression::op(expression::op_type::Var, varid, 0));

  return ecl_make_uint32_t(ref.get_id());
}

cl_object
clfun_varnames_extend(cl_object left, cl_object right) {
  if(SYMBOLP(left))
    left = cl_symbol_name(left);
  if(SYMBOLP(right))
    right = cl_symbol_name(right);

  if(ECL_STRINGP(left)) {
    expression::op_ref r;
    if(auto error = cl_object_conv(right, r)) {
      return *error;
    }

    auto ex = transform::variable_extend(ecl_string_to_string(left), "");
    auto res = ex(r);
    return ecl_make_uint32_t(res.get_id());
  } else if(ECL_STRINGP(right)) {
    expression::op_ref l;
    if(auto error = cl_object_conv(left, l)) {
      return *error;
    }

    auto ex = transform::variable_extend("", ecl_string_to_string(right));
    auto res = ex(l);
    return ecl_make_uint32_t(res.get_id());
  } else {
    return make_type_error("'left", "'string");
  }
}

cl_object
clfun_var_rename(cl_object op, cl_object oldname, cl_object newname) {
  expression::op_ref o;
  if(auto error = cl_object_conv(op, o)) {
    return *error;
  }

  std::string oldname_;
  std::string newname_;
  if(ECL_SYMBOLP(oldname))
    oldname = cl_symbol_name(oldname);
  if(ECL_STRINGP(oldname)) {
    oldname_ = ecl_string_to_string(oldname);
  } else if(ECL_FIXNUMP(oldname)) {
    expression::op_ref oldname_varop;
    if(auto error = cl_object_conv(oldname, oldname_varop)) {
      return *error;
    }
    if(oldname_varop->type != expression::op_type::Var) {
      return make_type_error("'oldname", "'string");
    }
    oldname_ = op_manager->vars()[oldname_varop->var.v]->name;
  }

  if(ECL_SYMBOLP(newname))
    newname = cl_symbol_name(newname);
  if(ECL_STRINGP(newname)) {
    newname_ = ecl_string_to_string(newname);
  } else if(ECL_FIXNUMP(newname)) {
    expression::op_ref newname_varop;
    if(auto error = cl_object_conv(newname, newname_varop)) {
      return *error;
    }
    if(newname_varop->type != expression::op_type::Var) {
      return make_type_error("'newname", "'string");
    }
    newname_ = op_manager->vars()[newname_varop->var.v]->name;
  }

  auto renamer =
    transform::variable_rename(op_manager->vars(), oldname_, newname_);
  auto renamed = renamer(o);
  return ecl_make_uint32_t(renamed.get_id());
}

cl_object
clfun_get_op_type(cl_object op) {
  expression::op_ref ref;
  if(auto error = cl_object_conv(op, ref)) {
    return *error;
  }

  return ecl_make_uint8_t(ref->type);
}

template<expression::op_ref (*B)(expression::op_ref, expression::op_ref)>
consteval static auto
makefun_binop() {
  return [](cl_object a, cl_object b) -> cl_object {
    expression::op_ref a_;
    if(auto error = cl_object_conv(a, a_)) {
      return *error;
    }
    expression::op_ref b_;
    if(auto error = cl_object_conv(b, b_)) {
      return *error;
    }

    expression::op_ref res = B(a_, b_);

    return ecl_make_uint32_t(res.get_id());
  };
}

static auto clfun_op_and =
  makefun_binop<[](auto a, auto b) { return a && b; }>();
static auto clfun_op_or =
  makefun_binop<[](auto a, auto b) { return a || b; }>();
static auto clfun_op_equi = makefun_binop<[](auto a, auto b) {
  return a.get_mgr().get(
    expression::op(expression::op_type::Equi, a.get_id(), b.get_id()));
}>();
static auto clfun_op_impl = makefun_binop<[](auto a, auto b) {
  return a.get_mgr().get(
    expression::op(expression::op_type::Impl, a.get_id(), b.get_id()));
}>();
static auto clfun_op_lpmi = makefun_binop<[](auto a, auto b) {
  return a.get_mgr().get(
    expression::op(expression::op_type::Lpmi, a.get_id(), b.get_id()));
}>();

template<expression::op_ref (*B)(expression::op_ref)>
consteval static auto
makefun_unop() {
  return [](cl_object o) -> cl_object {
    expression::op_ref o_;
    if(auto error = cl_object_conv(o, o_)) {
      return *error;
    }

    return ecl_make_uint32_t(B(o_).get_id());
  };
}

static auto clfun_op_not = makefun_unop<[](auto o) { return !o; }>();
static auto clfun_distribute_to_cnf =
  makefun_unop<[](auto o) { return transform::distribute_to_cnf(o); }>();
static auto clfun_distribute_ors =
  makefun_unop<[](auto o) { return transform::distribute_ors()(o); }>();
static auto clfun_distribute_nots =
  makefun_unop<[](auto o) { return transform::distribute_nots()(o); }>();

ecl_wrapper::ecl_wrapper() {
  char* argv[] = { NULL };
  cl_boot(0, argv);

  ecl_init_module(NULL, ECL_INIT_LIB_FUNC);

  clfun_eval = cl_eval(c_string_to_object("#'eval-sexp-and-catch-errors"));
  clfun_b_make_op = cl_eval(c_string_to_object("#'b-make-op"));
  clfun_b_define_global_last_op =
    cl_eval(c_string_to_object("#'b-define-global-last-op"));
  cltype_variable = c_string_to_object("variable");
  cltype_op = c_string_to_object("op");

  DEFUN("booleguru-get-varop-id", clfun_get_varop_id, 1);
  DEFUN("booleguru-op-type", clfun_get_op_type, 1);
  DEFUN("b-and", +clfun_op_and, 2);
  DEFUN("b-or", +clfun_op_or, 2);
  DEFUN("b-equi", +clfun_op_equi, 2);
  DEFUN("b-impl", +clfun_op_impl, 2);
  DEFUN("b-lpmi", +clfun_op_lpmi, 2);
  DEFUN("b-not", +clfun_op_not, 1);

  DEFUN("distribute-to-cnf", +clfun_distribute_to_cnf, 1);
  DEFUN("distribute-ors", +clfun_distribute_ors, 1);
  DEFUN("distribute-nots", +clfun_distribute_nots, 1);

  DEFUN("b-vars-extend", clfun_varnames_extend, 2);
  DEFUN("b-var-rename", clfun_var_rename, 3);

  DEFUN("booleguru-", clfun_get_varop_id, 1);
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
  cl_object form = c_string_to_object(code);

  if(last_op)
    cl_funcall(2, clfun_b_define_global_last_op, ecl_make_uint32_t(*last_op));

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
