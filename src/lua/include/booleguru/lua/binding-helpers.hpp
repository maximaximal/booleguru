#pragma once

#include <unordered_map>

#include <sol/variadic_args.hpp>

#include <booleguru/solve/result.hpp>

#include <booleguru/expression/id.hpp>
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
#include <booleguru/transform/prenex_quantifiers_optimal.hpp>
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

expression::op_ref
get_op_varop_q(const expression::op_ref& op);

expression::op_ref
get_op_quantop_v(const expression::op_ref& op);

expression::op_ref
get_op_quantop_e(const expression::op_ref& op);

bool
get_op_is_cnf(const expression::op_ref& r);

bool
get_op_is_prenex(const expression::op_ref& r);

bool
get_op_is_binop(const expression::op_ref& r);

bool
get_op_is_quantop(const expression::op_ref& r);

size_t
compute_variables_hash(const expression::op_ref& r);

expression::op_ref
rename(expression::op_ref& r,
       const std::string& oldname,
       const std::string& newname);

expression::op_ref
rename_vararg(expression::op_ref& r, sol::variadic_args va);

expression::op_ref
rename_map(expression::op_ref& r,
           const std::unordered_map<std::string, std::string>& map);

expression::op_ref
prefix_variables(expression::op_ref& r, const std::string& prefix);

expression::op_ref
suffix_variables(expression::op_ref& r, const std::string& suffix);

expression::op_ref
variables_set_i(expression::op_ref& r, uint16_t);

expression::op_ref
variables_set_q(expression::op_ref& r, uint16_t);

template<expression::op_type type>
static expression::op_ref
binop(expression::op_ref& l, expression::op_ref& r) {
  if(l.valid() && !r.valid()) [[unlikely]]
    return l;
  if(!l.valid() && r.valid()) [[unlikely]]
    return r;
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

template<class Transformer, expression::op_type op>
static expression::op_ref
quantify_op(expression::op_ref& o,
            std::optional<std::string> only_q,
            std::optional<std::string> only_i) {
  std::optional<uint16_t> only_q_i, only_i_i;
  if(only_q) {
    only_q_i = std::atoi(only_q->c_str());
  }
  if(only_i) {
    only_i_i = std::atoi(only_i->c_str());
  }
  
  return Transformer(op, only_q_i, only_i_i)(o);
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
  Transformer t(k);
  expression::op_ref out;
  try {
    out = t(o);
  } catch(const std::runtime_error& e) {
    fmt::println("Error during prenex-quantifier: {}", e.what());
    throw e;
  }
  return out;
}

template<class Transformer, transform::prenex_quantifier_optimal::kind k>
static expression::op_ref
transform_prenex_optimal_animated(expression::op_ref& o,
                                  std::string animate = "") {
  auto t = Transformer(k);
  if(animate != "") {
    t.animate(animate);
  }
  return t(o);
}

template<class Transformer, transform::prenex_quantifier_optimal::kind k>
expression::op_ref
transform_prenex_optimal(expression::op_ref& o) {
  Transformer t(k);
  expression::op_ref out;
  try {
    out = t(o);
  } catch(const std::runtime_error& e) {
    fmt::println("Error during prenex-quantifier-optimal: {}", e.what());
    throw e;
  }
  return out;
}

expression::op_ref
get_variable_from_manager(const std::string& name, expression::op_manager& mgr);

expression::op_ref
get_variable_from_global_handle(const std::string& name);

std::vector<expression::op_ref>
get_variables_from_global_handle(const std::string& names);

expression::op_ref
prenex(expression::op_ref o,
       transform::prenex_quantifier::kind kind,
       const std::string& animation_path = "");

expression::op_ref
prenex_optimal(expression::op_ref o,
               transform::prenex_quantifier_optimal::kind kind,
               const std::string& animation_path = "");

std::optional<expression::op_ref>
solve_sat(expression::op_ref o,
          std::string solver = "kissat",
          std::vector<std::string> args = { "-q" });

std::optional<std::unordered_map<uint32_t, bool>>
solve_sat_to_resultmap(expression::op_ref o,
                       std::string solver = "kissat",
                       std::vector<std::string> args = { "-q" });

inline std::optional<expression::op_ref>
solve_sat_default_args(expression::op_ref o) {
  return solve_sat(o);
}
}
