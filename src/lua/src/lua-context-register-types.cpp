#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

#include <set>

#include <booleguru/solve/result.hpp>

#include <booleguru/lua/lua-context.hpp>

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

using namespace booleguru::expression;

namespace booleguru::lua {

static op_type
get_op_ref_type(const op_ref& r) {
  return r->type;
}

static std::optional<op_ref>
get_op_left(const op_ref& r) {
  auto ref = r->left();
  if(ref == 0)
    return std::nullopt;
  return r.get_mgr()[ref];
}

static std::optional<op_ref>
get_op_right(const op_ref& r) {
  auto ref = r->right();
  if(ref == 0)
    return std::nullopt;
  return r.get_mgr()[ref];
}

static bool
get_op_and_inside(const op_ref& r) {
  return r->and_inside;
}

static bool
get_op_is_ors(const op_ref& r) {
  return r->is_ors;
}

static uint32_t
get_op_varop_v(const op_ref& op) {
  return op->var.v;
}

static uint32_t
get_op_varop_q(const op_ref& op) {
  return op->var.q;
}

static bool
get_op_is_cnf(const op_ref& r) {
  return r->is_cnf;
}

static size_t
compute_variables_hash(const op_ref& r) {
  std::set<int32_t> vars;
  transform::hash_variables hasher(vars);
  hasher(r);
  return hasher.hash();
}

static op_ref
rename(op_ref& r, const std::string& oldname, const std::string& newname) {
  return transform::variable_rename(r.get_mgr().vars(),
                                    { std::make_pair(oldname, newname) })(r);
}

static op_ref
rename_map(op_ref& r, const std::unordered_map<std::string, std::string>& map) {
  return transform::variable_rename(r.get_mgr().vars(), map)(r);
}

template<op_type type>
static op_ref
binop(op_ref& l, op_ref& r) {
  return l.get_mgr().get(op(type, l.get_id(), r.get_id()));
}

template<op_type type>
static op_ref
unop(op_ref& l) {
  return l.get_mgr().get(op(type, l.get_id(), 0));
}

template<class Transformer>
static op_ref
transform_op(op_ref& o) {
  return Transformer()(o);
}

static op_ref
get_variable_from_manager(const std::string& name, op_manager& mgr) {
  auto varref = mgr.vars().get(variable{ name });
  return mgr.get(op(op_type::Var, varref.get_id(), 0));
}

auto
fennel_s(sol::state& s, const std::string& fennel) -> auto{
  const std::string name = s["fennel"]["mangle"](fennel);
  return s[name];
}

void
set_to_state(sol::state& s,
             const std::string& lua,
             const std::string& fennel,
             auto&& f) {
  s[lua] = f;
  fennel_s(s, fennel) = f;
}

void
set_to_state(sol::state& s, const std::string& both, auto&& f) {
  set_to_state(s, both, both, f);
}

expression::op_ref
lua_context::get_var(const std::string& name) {
  return get_variable_from_manager(name, *ops_);
}

void
lua_context::register_booleguru_types() {
  sol::state& s = *state_;
  s.new_enum<solve::result::type>("result",
                                  { { "SAT", solve::result::SAT },
                                    { "UNSAT", solve::result::UNSAT },
                                    { "UNKNOWN", solve::result::UNKNOWN } });

  s.new_enum<expression::op_type>("optype",
                                  { { "none", op_type::None },
                                    { "exists", op_type::Exists },
                                    { "forall", op_type::Forall },
                                    { "equi", op_type::Equi },
                                    { "impl", op_type::Impl },
                                    { "lpmi", op_type::Lpmi },
                                    { "or_", op_type::Or },
                                    { "and_", op_type::And },
                                    { "xor", op_type::Xor },
                                    { "not_", op_type::Not },
                                    { "var", op_type::Var } });

  set_to_state(s,
               "eliminate_equivalence",
               "eliminate-equivalence",
               &transform_op<transform::eliminate_equivalence>);
  set_to_state(s,
               "eliminate_implication",
               "eliminate-implication",
               &transform_op<transform::eliminate_implication>);
  set_to_state(s,
               "distribute_nots",
               "distribute-nots",
               &transform_op<transform::distribute_nots>);
  set_to_state(s,
               "distribute_ors",
               "distribute-ors",
               &transform_op<transform::distribute_ors>);
  set_to_state(
    s, "distribute_to_cnf", "distribute-to-cnf", &transform::distribute_to_cnf);
  set_to_state(s, "vars_hash", "vars-hash", &compute_variables_hash);
  set_to_state(s, "b_var_rename", "b-var-rename", &rename);
  set_to_state(s, "tseitin", "tseitin", [this]() {
    return transform::tseitin<transform::output_to_op>(*ops_);
  }());

  set_to_state(
    s,
    "prenex_quantifier_Eup_Aup",
    "prenex-quantifier-Eup-Aup",
    &transform_op<
      transform::prenex_quantifier<transform::prenex_quantifier_Eup_Aup>>);
  set_to_state(
    s,
    "prenex_quantifier_Edown_Adown",
    "prenex-quantifier-Edown-Adown",
    &transform_op<
      transform::prenex_quantifier<transform::prenex_quantifier_Edown_Adown>>);
  set_to_state(
    s,
    "prenex_quantifier_Eup_Adown",
    "prenex-quantifier-Eup-Adown",
    &transform_op<
      transform::prenex_quantifier<transform::prenex_quantifier_Eup_Adown>>);
  set_to_state(
    s,
    "prenex_quantifier_Edown_Aup",
    "prenex-quantifier-Edown-Aup",
    &transform_op<
      transform::prenex_quantifier<transform::prenex_quantifier_Edown_Aup>>);

  const std::string bvar = s["fennel"]["mangle"]("b-var");
  s.set_function("b_var", &lua_context::get_var, this);
  s.set_function(bvar, &lua_context::get_var, this);
  s.set_function("v", &lua_context::get_var, this);

  auto op_type = s.new_usertype<op>("op");

  set_to_state(s, "exists", &binop<op_type::Exists>);
  set_to_state(s, "forall", &binop<op_type::Forall>);
  set_to_state(s, "equi", &binop<op_type::Equi>);
  set_to_state(s, "impl", &binop<op_type::Impl>);
  set_to_state(s, "lpmi", &binop<op_type::Lpmi>);
  set_to_state(s, "xor", &binop<op_type::Xor>);
  set_to_state(s, "b_and", "b-and", &binop<op_type::And>);
  set_to_state(s, "b_or", "b-or", &binop<op_type::Or>);
  set_to_state(s, "b_not", "b-not", &unop<op_type::Not>);

  using namespace booleguru::transform;
  auto reftype = s.new_usertype<op_ref>(
    "opref",
    "rename",
    rename,
    "rename",
    rename_map,
    sol::meta_function::bitwise_and,
    sol::resolve<op_ref(op_ref, op_ref)>(operator&&),
    sol::meta_function::bitwise_or,
    sol::resolve<op_ref(op_ref, op_ref)>(operator||),
    sol::meta_function::bitwise_not,
    sol::resolve<op_ref(op_ref)>(operator!),
    sol::meta_function::bitwise_xor,
    sol::resolve<op_ref(op_ref, op_ref)>(operator^),
    sol::meta_function::concatenation,
    sol::overload(
      sol::resolve<op_ref(op_ref&, int)>(operator+),
      sol::resolve<op_ref(op_ref&, const std::string&)>(operator+)));

  reftype.set("id", sol::property(&op_ref::get_id));
  reftype.set("t", sol::property(&get_op_ref_type));
  reftype.set("type", sol::property(&get_op_ref_type));
  reftype.set("l", sol::property(&get_op_left));
  reftype.set("left", sol::property(&get_op_left));
  reftype.set("child", sol::property(&get_op_left));
  reftype.set("r", sol::property(&get_op_right));
  reftype.set("right", sol::property(&get_op_right));

  reftype.set("v", sol::property(&get_op_varop_v));
  reftype.set("q", sol::property(&get_op_varop_q));

  reftype.set("and_inside", sol::property(&get_op_and_inside));
  reftype.set("is_ors", sol::property(&get_op_is_ors));
  reftype.set("is_cnf", sol::property(&get_op_is_cnf));
}
}
