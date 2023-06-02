#include <booleguru/lua/binding-helpers.hpp>

#include <booleguru/expression/literals.hpp>

namespace booleguru::lua::helpers {
uint32_t
get_op_id(const expression::op_ref& r) {
  return r.get_id();
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
  return op->var.v;
}

uint32_t
get_op_varop_q(const expression::op_ref& op) {
  return op->var.q;
}

bool
get_op_is_cnf(const expression::op_ref& r) {
  return r->is_cnf;
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
rename_map(expression::op_ref& r,
           const std::unordered_map<std::string, std::string>& map) {
  return transform::variable_rename(r.get_mgr().vars(), map)(r);
}

expression::op_ref
get_variable_from_manager(const std::string& name,
                          expression::op_manager& mgr) {
  auto varref = mgr.vars().get(expression::variable{ name });
  return mgr.get(expression::op(expression::op_type::Var, varref.get_id(), 0));
}

expression::op_ref
get_variable_from_global_handle(const std::string& name) {
  return expression::literals::proxy<expression::op_manager>(name);
}

expression::op_ref
prenex(expression::op_ref o,
       transform::prenex_quantifier::kind kind,
       const std::string& animation_path) {
  auto t = transform::prenex_quantifier(kind);
  t.animate(animation_path);
  return t(o);
}
}
