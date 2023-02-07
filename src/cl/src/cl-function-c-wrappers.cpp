#include <iostream>

#include <booleguru/cl/cl-function-c-wrappers.h>
#include <booleguru/cl/cl-globals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/transform/cnf.hpp>
#include <booleguru/transform/distribute_nots.hpp>
#include <booleguru/transform/distribute_ors.hpp>
#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/eliminate_implication.hpp>
#include <booleguru/transform/prenex_quantifiers.hpp>
#include <booleguru/transform/variable_extend.hpp>
#include <booleguru/transform/variable_rename.hpp>

namespace booleguru::cl {
std::shared_ptr<booleguru::expression::op_manager> op_manager;

static inline void
ensure_op() noexcept {
  if(!op_manager) {
    op_manager = std::make_shared<expression::op_manager>();
  }
}

template<expression::op_type t>
uint32_t inline binop(uint32_t l, uint32_t r) noexcept {
  ensure_op();
  if(l >= op_manager->size() || r >= op_manager->size())
    return 0;
  return op_manager->get(expression::op(t, l, r)).get_id();
}

template<typename F>
uint32_t inline transformer(F f, uint32_t op) noexcept {
  ensure_op();
  if(op > op_manager->size()) {
    return 0;
  }
  return f((*op_manager)[op]).get_id();
}
}

using namespace booleguru;
using namespace booleguru::cl;

extern "C" uint32_t
booleguru_cl_varop(const char* name) {
  ensure_op();
  if(!name)
    return 0;
  return op_manager
    ->get(expression::op(
      expression::op_type::Var,
      op_manager->vars().get(expression::variable{ name }).get_id(),
      0))
    .get_id();
}

extern "C" uint32_t
booleguru_cl_vars_prefix(uint32_t opid, const char* prefix) {
  ensure_op();
  if(opid >= op_manager->size() || !prefix)
    return 0;
  auto ex = transform::variable_extend(prefix, "");
  auto res = ex((*op_manager)[opid]);
  return res.get_id();
}

extern "C" uint32_t
booleguru_cl_vars_postfix(uint32_t opid, const char* postfix) {
  ensure_op();
  if(opid >= op_manager->size() || !postfix)
    return 0;
  auto ex = transform::variable_extend("", postfix);
  auto res = ex((*op_manager)[opid]);
  return res.get_id();
}

extern "C" uint32_t
booleguru_cl_vars_wrap(uint32_t opid, const char* prefix, const char* postfix) {
  ensure_op();
  if(opid >= op_manager->size() || !prefix || !postfix)
    return 0;
  auto ex = transform::variable_extend(prefix, postfix);
  auto res = ex((*op_manager)[opid]);
  return res.get_id();
}

extern "C" uint32_t
booleguru_cl_var_rename(uint32_t opid,
                        const char* oldname,
                        const char* newname) {
  ensure_op();
  if(opid >= op_manager->size() || !oldname || !newname)
    return 0;
  auto ex = transform::variable_rename(op_manager->vars(), oldname, newname);
  auto res = ex((*op_manager)[opid]);
  return res.get_id();
}

extern "C" uint32_t
booleguru_cl_optype(uint32_t opid) {
  ensure_op();
  if(opid >= op_manager->size())
    return 0;
  return static_cast<uint32_t>((*op_manager)[opid]->type);
}

extern "C" uint32_t
booleguru_cl_and(uint32_t l, uint32_t r) {
  return binop<expression::op_type::And>(l, r);
}

extern "C" uint32_t
booleguru_cl_or(uint32_t l, uint32_t r) {
  return binop<expression::op_type::Or>(l, r);
}

extern "C" uint32_t
booleguru_cl_equi(uint32_t l, uint32_t r) {
  return binop<expression::op_type::Equi>(l, r);
}

extern "C" uint32_t
booleguru_cl_impl(uint32_t l, uint32_t r) {
  return binop<expression::op_type::Impl>(l, r);
}

extern "C" uint32_t
booleguru_cl_lpmi(uint32_t l, uint32_t r) {
  return binop<expression::op_type::Lpmi>(l, r);
}

extern "C" uint32_t
booleguru_cl_not(uint32_t l) {
  return binop<expression::op_type::Not>(l, 0);
}

extern "C" uint32_t
booleguru_cl_distribute_to_cnf(uint32_t l) {
  return transformer(transform::distribute_to_cnf, l);
}

extern "C" uint32_t
booleguru_cl_distribute_or(uint32_t l) {
  return transformer(transform::distribute_ors(), l);
}

extern "C" uint32_t
booleguru_cl_distribute_not(uint32_t l) {
  return transformer(transform::distribute_nots(), l);
}

extern "C" uint32_t
booleguru_cl_distribute_implication(uint32_t l) {
  return transformer(transform::eliminate_implication(), l);
}

extern "C" uint32_t
booleguru_cl_distribute_equivalence(uint32_t l) {
  return transformer(transform::eliminate_equivalence(), l);
}

extern "C" uint32_t
booleguru_cl_prenex_quantifier(uint32_t l, uint32_t variant) {
  switch(variant) {
    case 0:
      return transformer(transform::prenex_quantifier<
                           transform::prenex_quantifier_Edown_Adown>(),
                         l);
    case 1:
      return transformer(
        transform::prenex_quantifier<transform::prenex_quantifier_Edown_Aup>(),
        l);
    case 2:
      return transformer(
        transform::prenex_quantifier<transform::prenex_quantifier_Eup_Adown>(),
        l);
    case 3:
      return transformer(
        transform::prenex_quantifier<transform::prenex_quantifier_Eup_Aup>(),
        l);
    default:
      std::cerr << "Variant must be 0 (Edown Adown), 1 (Edown Aup), 2 (Eup "
                   "Adown), or 3 (Eup Aup)!"
                << std::endl;
      return 0;
  }
}

extern "C" void
booleguru_cl_print(uint32_t op) {
  ensure_op();
  if(op >= op_manager->size()) {
    return;
  }
  std::cout << (*op_manager)[op] << std::endl;
}
