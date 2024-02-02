#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

#include <set>

#include <booleguru/lua/binding-helpers.hpp>
#include <booleguru/lua/lua-context.hpp>

#include <booleguru/transform/minimize_failing.hpp>
#include <booleguru/transform/variable_extend.hpp>

#include <booleguru/transform/eliminate_xor.hpp>

using namespace booleguru::expression;
using namespace booleguru::lua::helpers;

namespace booleguru::lua {

expression::op_ref
lua_context::get_var(const std::string& name) {
  return get_variable_from_manager(name, *ops_);
}

static void
set_to_state(sol::state& s,
             const std::string& lua,
             const std::string& fennel,
             auto&& f) {
  s[lua] = f;
  if(lua != fennel)
    s.script("package.preload['" + fennel + "'] = function() return " + lua
             + " end");
}

static void
set_to_state(sol::state& s, const std::string& both, auto&& f) {
  set_to_state(s, both, both, f);
}

static std::optional<expression::op_ref>
solve_sat_variadic(expression::op_ref o,
                   std::string solver,
                   sol::variadic_args va) {
  std::vector<std::string> args;
  std::transform(va.begin(),
                 va.end(),
                 std::back_inserter(args),
                 [](const auto& lua_arg) -> std::string { return lua_arg; });
  return helpers::solve_sat(o, solver, args);
}

#define xstr(s) str(s)
#define str(s) #s

#define BIND_PRENEX(L, F)                                      \
  set_to_state(                                                \
    s,                                                         \
    "linearize_quants_legacy_" str(L),                         \
    "linearize-quants-legacy-" F,                              \
    sol::overload(                                             \
      &transform_prenex<transform::prenex_quantifier,          \
                        transform::prenex_quantifier::L>,      \
      &transform_prenex_animated<transform::prenex_quantifier, \
                                 transform::prenex_quantifier::L>));

#define BIND_PRENEX_OPTIMAL(L, F)                                         \
  set_to_state(                                                           \
    s,                                                                    \
    "linearize_quants_" str(L),                                           \
    "linearize-quants-" F,                                                \
    sol::overload(                                                        \
      &transform_prenex_optimal<transform::prenex_quantifier_optimal,     \
                                transform::prenex_quantifier_optimal::L>, \
      &transform_prenex_optimal_animated<                                 \
        transform::prenex_quantifier_optimal,                             \
        transform::prenex_quantifier_optimal::L>));

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
               "eliminate_xor",
               "eliminate-xor",
               &transform_op<transform::eliminate_xor>);
  set_to_state(s,
               "distribute_nots",
               "distribute-nots",
               &transform_op<transform::distribute_nots>);
  set_to_state(s,
               "distribute_ors",
               "distribute-ors",
               &transform_op<transform::distribute_ors>);
  set_to_state(s,
               "minimize_failing",
               "minimize-failing",
               &transform_op<transform::minimize_failing>);
  set_to_state(
    s, "distribute_to_cnf", "distribute-to-cnf", &transform::distribute_to_cnf);
  set_to_state(s, "vars_hash", "vars-hash", &compute_variables_hash);
  set_to_state(s, "rename", "rename", &helpers::rename_vararg);
  set_to_state(s, "tseitin", "tseitin", [this]() {
    return transform::tseitin<transform::output_to_op>(*ops_);
  }());

  BIND_PRENEX(Eup_Aup, "Eup-up")
  BIND_PRENEX(Eup_Adown, "Eup-down")
  BIND_PRENEX(Edown_Aup, "Edown-up")
  BIND_PRENEX(Edown_Adown, "Edown-down")

  BIND_PRENEX_OPTIMAL(Eup_up, "Eup-up")
  BIND_PRENEX_OPTIMAL(Eup_down, "Eup-down")
  BIND_PRENEX_OPTIMAL(Edown_up, "Edown-up")
  BIND_PRENEX_OPTIMAL(Edown_down, "Edown-down")

  BIND_PRENEX_OPTIMAL(Aup_up, "Aup-up")
  BIND_PRENEX_OPTIMAL(Aup_down, "Aup-down")
  BIND_PRENEX_OPTIMAL(Adown_up, "Adown-up")
  BIND_PRENEX_OPTIMAL(Adown_down, "Adown-down")

  const std::string bvar = s["fennel"]["mangle"]("b-var");
  s.set_function("b_var", &lua_context::get_var, this);
  s.set_function(bvar, &lua_context::get_var, this);
  s.set_function("v", &lua_context::get_var, this);

  auto op_type = s.new_usertype<op>("op");

  set_to_state(s, "exists", &helpers::binop<op_type::Exists>);
  set_to_state(s, "forall", &helpers::binop<op_type::Forall>);
  set_to_state(s, "equi", &helpers::binop<op_type::Equi>);
  set_to_state(s, "impl", &helpers::binop<op_type::Impl>);
  set_to_state(s, "lpmi", &helpers::binop<op_type::Lpmi>);
  set_to_state(s, "xor", &helpers::binop<op_type::Xor>);
  set_to_state(s, "b_and", &helpers::binop<op_type::And>);
  s.set_function("b_or", &helpers::binop<op_type::Or>);
  const std::string bor = s["fennel"]["mangle"]("b-or");
  s.set_function(bor, &helpers::binop<op_type::Or>);
  s.set_function("b_and", &helpers::binop<op_type::And>);
  const std::string band = s["fennel"]["mangle"]("b-and");
  s.set_function(band, &helpers::binop<op_type::And>);
  s.set_function("b_not", &helpers::unop<op_type::Not>);
  const std::string bnot = s["fennel"]["mangle"]("b-not");
  s.set_function(bnot, &helpers::unop<op_type::Not>);

  set_to_state(s, "noop", [](expression::op_ref& o) { return o; });

  set_to_state(
    s,
    "solve",
    sol::overload(&helpers::solve_sat_default_args, &solve_sat_variadic));

  set_to_state(s, "prefix_vars", "prefix-vars", &helpers::prefix_variables);

  using namespace booleguru::transform;
  auto reftype = s.new_usertype<op_ref>(
    "opref",
    "rename",
    &helpers::rename_vararg,
    "rename_map",
    &helpers::rename_map,
    sol::meta_function::multiplication,
    sol::resolve<op_ref(op_ref, op_ref)>(operator&&),
    sol::meta_function::addition,
    sol::resolve<op_ref(op_ref, op_ref)>(operator||),
    sol::meta_function::concatenation,
    sol::resolve<op_ref(op_ref&, const std::string&)>(operator+),
    sol::meta_function::unary_minus,
    sol::resolve<op_ref(op_ref)>(operator!),
    sol::meta_function::division,
    sol::resolve<op_ref(op_ref, op_ref)>(operator^),
    sol::meta_function::concatenation,
    sol::overload(
      sol::resolve<op_ref(op_ref&, int)>(operator+),
      sol::resolve<op_ref(op_ref&, const std::string&)>(operator+)));

  reftype.set("id", sol::property(&get_op_id));
  reftype.set("t", sol::property(&get_op_ref_type));
  reftype.set("type", sol::property(&get_op_ref_type));
  reftype.set("l", sol::property(&get_op_left));
  reftype.set("left", sol::property(&get_op_left));
  reftype.set("child", sol::property(&get_op_left));
  reftype.set("r", sol::property(&get_op_right));
  reftype.set("right", sol::property(&get_op_right));

  reftype.set("v", sol::property(&get_op_varop_v));
  reftype.set("q", sol::property(&get_op_varop_q));

  reftype.set("qv", sol::property(&get_op_quantop_v));
  reftype.set("qe", sol::property(&get_op_quantop_e));

  reftype.set("and_inside", sol::property(&get_op_and_inside));
  reftype.set("is_ors", sol::property(&get_op_is_ors));
  reftype.set("is_cnf", sol::property(&get_op_is_cnf));
  reftype.set("is_prenex", sol::property(&get_op_is_prenex));
  reftype.set("is_quantop", sol::property(&get_op_is_quantop));
  reftype.set("is_binop", sol::property(&get_op_is_binop));
}
}
