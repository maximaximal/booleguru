#pragma once

#include <booleguru/solve/result.hpp>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/transform/cnf.hpp>
#include <booleguru/transform/distribute_nots.hpp>
#include <booleguru/transform/distribute_ors.hpp>
#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/eliminate_implication.hpp>
#include <booleguru/transform/hash_variables.hpp>
#include <booleguru/transform/output_to_op.hpp>
#include <booleguru/transform/prenex_quantifiers.hpp>
#include <booleguru/transform/tseitin.hpp>
#include <booleguru/transform/variable_extend.hpp>
#include <booleguru/transform/variable_rename.hpp>

namespace booleguru::lua::helpers {
uint32_t
get_op_id(const expression::op_ref& r);

expression::op_type
get_op_ref_type(const expression::op_ref& r);

std::optional<expression::op_ref>
get_op_left(const expression::op_ref& r);

std::optional<expression::op_ref>
get_op_right(const expression::op_ref& r);

bool
get_op_and_inside(const expression::op_ref& r);

bool
get_op_is_ors(const expression::op_ref& r);

uint32_t
get_op_varop_v(const expression::op_ref& op);

uint32_t
get_op_varop_q(const expression::op_ref& op);

bool
get_op_is_cnf(const expression::op_ref& r);

size_t
compute_variables_hash(const expression::op_ref& r);

expression::op_ref
rename(expression::op_ref& r,
       const std::string& oldname,
       const std::string& newname);

expression::op_ref
rename_map(expression::op_ref& r,
           const std::unordered_map<std::string, std::string>& map);

template<expression::op_type type>
static expression::op_ref
binop(expression::op_ref& l, expression::op_ref& r) {
  return l.get_mgr().get(expression::op(type, l.get_id(), r.get_id()));
}

template<expression::op_type type>
static expression::op_ref
unop(expression::op_ref& l) {
  return l.get_mgr().get(expression::op(type, l.get_id(), 0));
}

template<class Transformer>
static expression::op_ref
transform_op(expression::op_ref& o) {
  return Transformer()(o);
}

template<class Transformer, transform::prenex_quantifier::kind k>
static expression::op_ref
transform_prenex_animated(expression::op_ref& o, std::string animate = "") {
  auto t = Transformer(k);
  if(animate != "") {
    t.animate(animate);
  }
  return t(o);
}

template<class Transformer, transform::prenex_quantifier::kind k>
expression::op_ref
transform_prenex(expression::op_ref& o) {
  return Transformer(k)(o);
}

expression::op_ref
get_variable_from_manager(const std::string& name, expression::op_manager& mgr);

expression::op_ref
get_variable_from_global_handle(const std::string& name);

expression::op_ref
prenex(expression::op_ref o,
       transform::prenex_quantifier::kind kind,
       const std::string& animation_path = "");
}