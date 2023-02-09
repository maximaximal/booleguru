#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/transform/prenex_quantifiers.hpp>

using namespace booleguru::expression;
using namespace booleguru::transform;
using namespace booleguru::expression::literals;

TEST_CASE("Transform a simple Non-Prenex formula into prenex formula") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_v1 = ops->vars().get(variable{ "p" });
  auto var_v2 = ops->vars().get(variable{ "q" });
  auto var_v3 = ops->vars().get(variable{ "r" });

  auto op_v1 = ops->get(op(op_type::Var, var_v1.get_id(), 0));
  auto op_v2 = ops->get(op(op_type::Var, var_v2.get_id(), 0));
  auto op_v3 = ops->get(op(op_type::Var, var_v3.get_id(), 0));

  auto op_g1 = op_v1 || op_v2 || op_v3;
  auto op_g2 = !op_v1 || op_v2 || op_v3;

  auto op_g3 = ops->get(op(op_type::Exists, op_v3.get_id(), op_g1.get_id()));
  auto op_g4 = ops->get(op(op_type::Forall, op_v2.get_id(), op_g3.get_id()));

  auto op_g5 = ops->get(op(op_type::Forall, op_v2.get_id(), op_g2.get_id()));
  auto op_g6 = ops->get(op(op_type::Exists, op_v3.get_id(), op_g5.get_id()));

  auto op_g7 = op_g4 && op_g6;
  auto op_g8 = ops->get(op(op_type::Exists, op_v1.get_id(), op_g7.get_id()));

  std::stringstream serialized;
  serialized << op_g8;

  CAPTURE(serialized.str());

  prenex_quantifier p;
  p(op_g8);

  std::stringstream transformed;
  transformed << p(op_g8);

  CAPTURE(transformed.str());

  const char* expected =
    R"(?p[14] ?r[12] #q[10] #q[11] ?r[9] ((p[14] | q[10] | r[9]) & (!p[14] | q[11] | r[12])))";

  REQUIRE(transformed.str() == expected);
}

struct prenex_test_variant {
  using F = std::function<op_ref(op_ref)>;
  F transform;
  std::string match_result;
  std::string name;

  template<class T>
  static prenex_test_variant gen(std::string name, std::string match_result) {
    return prenex_test_variant{ prenex_quantifier<T>(), match_result, name };
  }
};

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "with multiple prenexing variants"
          "(check negation of quantifiers") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto v = GENERATE(
    prenex_test_variant::gen<prenex_quantifier_Eup_Aup>(
      "Eup Aup",
      "?p[29] ?q''[25] #q[21] #q'[23] #r''[24] ?r[20] ?r'[22] #s[19] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Edown_Adown>(
      "Edown Adown",
      "?p[29] #q[21] ?q''[25] ?r[20] #q'[23] #r''[24] #s[19] ?r'[22] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Eup_Adown>(
      "Eup Adown",
      "?p[29] ?q''[25] #q[21] #q'[23] ?r[20] ?r'[22] #s[19] #r''[24] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Edown_Aup>(
      "Edown Aup",
      "?p[29] ?q''[25] #q[21] #q'[23] #r''[24] ?r[20] #s[19] ?t[18] ?r'[22] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"));

  auto var_v2 = ops->vars().get(variable{ "q" });
  auto var_v3 = ops->vars().get(variable{ "r" });
  auto var_v4 = ops->vars().get(variable{ "s" });
  auto var_v5 = ops->vars().get(variable{ "t" });
  auto var_v6 = ops->vars().get(variable{ "q'" });
  auto var_v7 = ops->vars().get(variable{ "r'" });
  auto var_v8 = ops->vars().get(variable{ "q''" });
  auto var_v9 = ops->vars().get(variable{ "r''" });

  auto op_v1 = "p"_var(ops);
  auto op_v2 = ops->get(op(op_type::Var, var_v2.get_id(), 0));
  auto op_v3 = ops->get(op(op_type::Var, var_v3.get_id(), 0));
  auto op_v4 = ops->get(op(op_type::Var, var_v4.get_id(), 0));
  auto op_v5 = ops->get(op(op_type::Var, var_v5.get_id(), 0));
  auto op_v6 = ops->get(op(op_type::Var, var_v6.get_id(), 0));
  auto op_v7 = ops->get(op(op_type::Var, var_v7.get_id(), 0));
  auto op_v8 = ops->get(op(op_type::Var, var_v8.get_id(), 0));
  auto op_v9 = ops->get(op(op_type::Var, var_v9.get_id(), 0));

  auto op_g1 = op_v1 || op_v2 || op_v3 || op_v4 || op_v5;
  auto op_g2 = op_v1 || op_v6 || op_v7;
  auto op_g3 = op_v1 || op_v8 || op_v9;

  auto op_g4 = ops->get(op(op_type::Exists, op_v5.get_id(), op_g1.get_id()));
  auto op_g5 = ops->get(op(op_type::Forall, op_v4.get_id(), op_g4.get_id()));
  auto op_g6 = ops->get(op(op_type::Exists, op_v3.get_id(), op_g5.get_id()));
  auto op_g7 = ops->get(op(op_type::Forall, op_v2.get_id(), op_g6.get_id()));

  auto op_g8 = ops->get(op(op_type::Exists, op_v7.get_id(), op_g2.get_id()));
  auto op_g9 = ops->get(op(op_type::Forall, op_v6.get_id(), op_g8.get_id()));

  auto op_g10 = ops->get(op(op_type::Exists, op_v9.get_id(), op_g3.get_id()));
  auto op_g11 = ops->get(op(op_type::Forall, op_v8.get_id(), op_g10.get_id()));
  auto op_g12 = ops->get(op(op_type::Not, op_g11.get_id(), 0));

  auto op_g13 = op_g7 && op_g9 && op_g12;

  auto op_g14 = ops->get(op(op_type::Exists, op_v1.get_id(), op_g13.get_id()));

  auto result = v.transform(op_g14);

  CAPTURE(v.name);

  REQUIRE(result.to_string() == v.match_result);
}

TEST_CASE("Prenex a formula with multiple sub-trees with quantified variables "
          "of the same name") {
  op_manager ops;
  op_ref x = "x"_var(ops);
  op_ref y = "y"_var(ops);

  op_ref sub_tree_1 = forall(x, exists(y, x && y));
  op_ref sub_tree_2 = exists(y, forall(x, x && y));
  op_ref sub_tree_3 = !exists(y, forall(x, x && y));

  op_ref formula = sub_tree_1 && sub_tree_2 && sub_tree_3;

  prenex_quantifier<prenex_quantifier_Eup_Aup> prenexer;
  op_ref prenexed = prenexer(formula);

  CAPTURE(prenexed.to_string());

  REQUIRE(prenexed.to_string() == "#x[5] #x[6] #y[7] ?y[4] ?x[8] ?y[9] (x[5] & "
                                  "y[4] & x[6] & y[7] & !(x[8] & y[9]))");
}

TEST_CASE("Transform a Non-Prenex formula into prenex") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  op_ref s = "s"_var(ops);
  op_ref t = "t"_var(ops);

  auto formula_1 =
    forall(q, exists(r, forall(s, exists(t, p || q || r || s || t))));
  auto formula_2 = forall(q, exists(r, (q || q || r)));
  auto formula_3 = forall(q, exists(r, (q || q || r)));

  auto formula = exists(p, formula_1 && formula_2 && !(formula_3));

  prenex_quantifier pren;
  auto prenexed = pren(formula);

  REQUIRE(prenexed.to_string() ==
          "?p #q ?r #s ?t ?p #q' ?r' ?p ?q'' #r'' ((p | q | r | s | t) & (p | "
          "q' | r') & !(p | q'' | r''))");
}

TEST_CASE("Transform a simple Non-Prenex NCF formula into prenex formula "
          "with multiple prenexing variants") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto v = GENERATE(
    prenex_test_variant::gen<prenex_quantifier_Eup_Aup>(
      "Eup Aup",
      "?p[29] ?q''[25] #q[21] #q'[23] #r''[24] ?r[20] ?r'[22] #s[19] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Edown_Adown>(
      "Edown Adown",
      "?p[29] #q[21] ?q''[25] ?r[20] #q'[23] #r''[24] #s[19] ?r'[22] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Eup_Adown>(
      "Eup Adown",
      "?p[29] ?q''[25] #q[21] #q'[23] ?r[20] ?r'[22] #s[19] #r''[24] ?t[18] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"),
    prenex_test_variant::gen<prenex_quantifier_Edown_Aup>(
      "Edown Aup",
      "?p[29] ?q''[25] #q[21] #q'[23] #r''[24] ?r[20] #s[19] ?t[18] ?r'[22] "
      "((p[29] | q[21] | r[20] | s[19] | t[18]) & (p[29] | q'[23] | r'[22]) & "
      "!(p[29] | q''[25] | r''[24]))"));

  op_ref s0 = "s0"_var(ops);
  op_ref s1 = "s1"_var(ops);
  op_ref s2 = "s2"_var(ops);
  op_ref s3 = "s3"_var(ops);

  op_ref w = "w"_var(ops);

  op_ref v0 = "v0"_var(ops);
  op_ref v1 = "v1"_var(ops);
  op_ref v2 = "v2"_var(ops);
  op_ref v3 = "v3"_var(ops);

  op_ref u0 = "u0"_var(ops);
  op_ref u1 = "u1"_var(ops);
  op_ref u2 = "u2"_var(ops);
  op_ref u3 = "u3"_var(ops);

  op_ref v_0 = "v_0"_var(ops);
  op_ref v_1 = "v_1"_var(ops);
  op_ref v_2 = "v_2"_var(ops);
  op_ref v_3 = "v_3"_var(ops);

  auto op_p0 = exists(v0, s0) && forall(u0, impl(u0, forall(v_0, v_0)));
  auto op_p1 = forall(v1, s1) && exists(u1, impl(u1, exists(v_1, v_1)));
  auto op_p2 = exists(v2, s2) && forall(u2, impl(u2, forall(v_2, v_2)));
  auto op_p3 = forall(v3, s3) && exists(u3, impl(u3, exists(v_3, v_3)));

  auto op_formula = exists(s0, op_p0 && forall(s1, impl(op_p1, exists(s2, op_p2 && forall(s3, impl(op_p3, forall(w, w)))))));
  
  auto result = v.transform(op_formula);

  CAPTURE(v.name);
  CAPTURE(op_formula.to_string());

  REQUIRE(result.to_string() == v.match_result);
}
