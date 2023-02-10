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
#include <booleguru/transform/prenex_quantifiers.hpp>
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
binop(op_ref l, op_ref r) {
  return l.get_mgr().get(op(type, l.get_id(), r.get_id()));
}

template<op_type type>
static op_ref
unop(op_ref l) {
  return l.get_mgr().get(op(type, l.get_id(), 0));
}

template<class Transformer>
static op_ref
transform_op(op_ref o) {
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
                                    { "or", op_type::Or },
                                    { "and", op_type::And },
                                    { "xor", op_type::Xor },
                                    { "not", op_type::Not },
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

  auto get_var = [this](const std::string& name) {
    return get_variable_from_manager(name, *ops_);
  };
  fennel_s(s, "b-var") = get_var;
  fennel_s(s, "v") = get_var;
  s["v"] = get_var;

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
}
}
