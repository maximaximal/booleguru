#include "booleguru/transform/variable_extend.hpp"
#include "booleguru/transform/variable_set_i.hpp"
#include "booleguru/transform/variable_set_q.hpp"
#include <booleguru/lua/binding-helpers.hpp>
#include <booleguru/transform/tseitin.hpp>
#include <booleguru/util/istringviewstream.hpp>

#include <booleguru/expression/literals.hpp>

#include <booleguru/solve/sat.hpp>

namespace booleguru::lua::helpers {
uint32_t
get_op_id(const expression::op_ref& r) {
  return static_cast<uint32_t>(r.get_id());
}

expression::op_type
get_op_ref_type(const expression::op_ref& r) {
  return r->type;
}

std::optional<expression::op_ref>
get_op_left(const expression::op_ref& r) {
  auto ref = r->left();
  if(ref == 0)
    return std::nullopt;
  return r.get_mgr()[ref];
}

std::optional<expression::op_ref>
get_op_right(const expression::op_ref& r) {
  auto ref = r->right();
  if(ref == 0)
    return std::nullopt;
  return r.get_mgr()[ref];
}

bool
get_op_and_inside(const expression::op_ref& r) {
  return r->and_inside;
}

bool
get_op_is_ors(const expression::op_ref& r) {
  return r->is_ors;
}

uint32_t
get_op_varop_v(const expression::op_ref& op) {
  return static_cast<uint32_t>(op->var.v.id_);
}

expression::op_ref
get_op_varop_q(const expression::op_ref& op) {
  return op.get_mgr()[op->var.q];
}

expression::op_ref
get_op_quantop_v(const expression::op_ref& op) {
  return op.get_mgr()[op->quant.v];
}

expression::op_ref
get_op_quantop_e(const expression::op_ref& op) {
  return op.get_mgr()[op->quant.e];
}

bool
get_op_is_cnf(const expression::op_ref& r) {
  return r->is_cnf;
}

bool
get_op_is_prenex(const expression::op_ref& r) {
  return r->is_prenex;
}

bool
get_op_is_binop(const expression::op_ref& r) {
  return r->is_binop();
}

bool
get_op_is_quantop(const expression::op_ref& r) {
  return r->is_quant();
}

size_t
compute_variables_hash(const expression::op_ref& r) {
  std::set<int32_t> vars;
  transform::hash_variables hasher(vars);
  hasher(r);
  return hasher.hash();
}

expression::op_ref
rename(expression::op_ref& r,
       const std::string& oldname,
       const std::string& newname) {
  return transform::variable_rename(r.get_mgr().vars(),
                                    { std::make_pair(oldname, newname) })(r);
}

expression::op_ref
rename_vararg(expression::op_ref& r, sol::variadic_args va) {
  int i = 0;
  std::string a;

  std::unordered_map<std::string, std::string> renames;

  for(auto b_arg : va) {
    std::string b = b_arg;
    if(++i % 2 == 0) {
      renames[a] = b;
    } else {
      a = b;
    }
  }

  return transform::variable_rename(r.get_mgr().vars(), renames)(r);
}

expression::op_ref
rename_map(expression::op_ref& r,
           const std::unordered_map<std::string, std::string>& map) {
  return transform::variable_rename(r.get_mgr().vars(), map)(r);
}

expression::op_ref
prefix_variables(expression::op_ref& r, const std::string& prefix) {
  return transform::variable_extend(prefix, "")(r);
}

expression::op_ref
suffix_variables(expression::op_ref& r, const std::string& suffix) {
  return transform::variable_extend("", suffix)(r);
}

expression::op_ref
variables_set_i(expression::op_ref& r, uint16_t i) {
  return transform::variable_set_i(i)(r);
}

expression::op_ref
variables_set_q(expression::op_ref& r, uint16_t q) {
  return transform::variable_set_q(q)(r);
}

expression::op_ref
get_variable_from_manager(const std::string& name,
                          expression::op_manager& mgr) {
  auto varref = mgr.vars().get(expression::variable{ name });
  return mgr.get(
    expression::op(expression::op_type::Var, varref.get_id(), 0, 0));
}

expression::op_ref
get_variable_from_global_handle(const std::string& name) {
  return expression::literals::proxy<expression::op_manager>(name);
}

std::vector<expression::op_ref>
get_variables_from_global_handle(const std::string& names) {
  isviewstream in(names);
  std::vector<expression::op_ref> ops;
  std::string name;
  while(std::getline(in, name, ' ')) {
    if(name.size() > 0) {
      ops.emplace_back(get_variable_from_global_handle(name));
    }
  }
  return ops;
}

expression::op_ref
prenex(expression::op_ref o,
       transform::prenex_quantifier::kind kind,
       const std::string& animation_path) {
  auto t = transform::prenex_quantifier(kind);
  t.animate(animation_path);
  return t(o);
}

expression::op_ref
prenex_optimal(expression::op_ref o,
               transform::prenex_quantifier_optimal::kind kind,
               const std::string& animation_path) {
  auto t = transform::prenex_quantifier_optimal(kind);
  t.animate(animation_path);
  return t(o);
}

std::optional<expression::op_ref>
solve_sat(expression::op_ref o,
          std::string solver,
          std::vector<std::string> args) {
  solve::sat s(solver, args);
  return s.solve(o);
}

std::optional<std::unordered_map<uint32_t, bool>>
solve_sat_to_resultmap(expression::op_ref o,
                       std::string solver,
                       std::vector<std::string> args) {
  solve::sat s(solver, args);
  return s.solve_resultmap(o);
}
}
