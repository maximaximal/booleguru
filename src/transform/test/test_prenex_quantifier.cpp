#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include <booleguru/util/istringviewstream.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/transform/prenex_quantifiers.hpp>

using namespace booleguru::expression;
using namespace booleguru::parse;
using namespace booleguru::transform;
using namespace booleguru::expression::literals;

static bool activate_animations = false;

TEST_CASE("Transform a simple Non-Prenex formula into prenex formula") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_v1 = ops->vars().get(variable{ "p" });
  auto var_v2 = ops->vars().get(variable{ "q" });
  auto var_v3 = ops->vars().get(variable{ "r" });

  auto op_v1 = ops->get(op(op_type::Var, var_v1.get_id(), 0, 0));
  auto op_v2 = ops->get(op(op_type::Var, var_v2.get_id(), 0, 0));
  auto op_v3 = ops->get(op(op_type::Var, var_v3.get_id(), 0, 0));

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
  if(activate_animations)
    p.animate("test_simple");
  op_ref prenexed = p(op_g8);

  CAPTURE(prenexed.to_string());

  std::stringstream transformed;
  transformed << prenexed;

  const char* expected
    = R"(?p ?r[1] #q #q[1] ?r ((p | q | r) & (!p | q[1] | r[1])))";

  REQUIRE(transformed.str() == expected);
}

op_ref
build_formula_to_be_prenexed(std::shared_ptr<op_manager> ops) {
  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  op_ref s = "s"_var(ops);
  op_ref t = "t"_var(ops);
  op_ref q_ = "q'"_var(ops);
  op_ref r_ = "r'"_var(ops);
  op_ref q__ = "q''"_var(ops);
  op_ref r__ = "r''"_var(ops);
  auto formula_1
    = forall(q, exists(r, forall(s, exists(t, p || q || r || s || t))));
  auto formula_2 = forall(q_, exists(r_, (p || q_ || r_)));
  auto formula_3 = forall(q__, exists(r__, (p || q__ || r__)));
  auto formula = exists(p, formula_1 && formula_2 && !(formula_3));
  return formula;
}

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "with Eup Aup") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto formula = build_formula_to_be_prenexed(ops);

  const char* expected = "?p ?q'' #q #r'' #q' ?r ?r' "
                         "#s ?t ((p | q | r | s | t) & "
                         "(p | q' | r') & !(p | q'' | r''))";

  prenex_quantifier p(prenex_quantifier::Eup_Aup);
  if(activate_animations)
    p.animate("test_EupAup");
  op_ref prenexed = p(formula);

  REQUIRE(prenexed.to_string() == expected);
}

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "with Edown Aup") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto formula = build_formula_to_be_prenexed(ops);

  const char* expected = "?p #q #q' ?r ?q'' #s #r'' ?t "
                         "?r' ((p | q | r | s | t) & (p | "
                         "q' | r') & !(p | q'' | r''))";

  prenex_quantifier p(prenex_quantifier::Edown_Aup);
  if(activate_animations)
    p.animate("test_EdownAup");
  op_ref prenexed = p(formula);

  REQUIRE(prenexed.to_string() == expected);
}

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "with Eup Adown") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto formula = build_formula_to_be_prenexed(ops);

  const char* expected = "?p ?q'' #q ?r #s #r'' #q' ?t "
                         "?r' ((p | q | r | s | t) & (p | "
                         "q' | r') & !(p | q'' | r''))";

  prenex_quantifier p(prenex_quantifier::Eup_Adown);
  if(activate_animations)
    p.animate("test_EupAdown");
  op_ref prenexed = p(formula);

  REQUIRE(prenexed.to_string() == expected);
}

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "with Edown Adown") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto formula = build_formula_to_be_prenexed(ops);

  const char* expected = "?p #q ?r ?q'' #s #q' #r'' ?t "
                         "?r' ((p | q | r | s | t) & (p | "
                         "q' | r') & !(p | q'' | r''))";

  prenex_quantifier p(prenex_quantifier::Edown_Adown);
  if(activate_animations)
    p.animate("test_EdownAdown");
  op_ref prenexed = p(formula);

  REQUIRE(prenexed.to_string() == expected);
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

  CAPTURE(formula.to_string());

  prenex_quantifier prenexer(prenex_quantifier::Edown_Aup);
  if(activate_animations)
    prenexer.animate("multiple_subtrees_Edown_Aup");
  op_ref prenexed = prenexer(formula);

  CAPTURE(prenexed.to_string());

  REQUIRE(prenexed.to_string()
          == "#y[2] #x ?x[2] ?y ?y[1] #x[1] (x & "
             "y & x[1] & y[1] & !(x[2] & y[2]))");
}

TEST_CASE("Transform a Non-Prenex formula into prenex") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  op_ref s = "s"_var(ops);
  op_ref t = "t"_var(ops);

  auto formula_1
    = forall(q, exists(r, forall(s, exists(t, p || q || r || s || t))));
  auto formula_2 = forall(q, exists(r, (p || q || r)));
  auto formula_3 = forall(q, exists(r, (p || q || r)));

  auto formula = exists(p, formula_1 && formula_2 && !(formula_3));

  prenex_quantifier pren;
  auto prenexed = pren(formula);

  REQUIRE(prenexed.to_string()
          == "?p ?q[2] #q #r[2] #q[1] ?r ?r[1] #s ?t ((p | q | "
             "r | s | t) & (p | q[1] | r[1]) & !(p | q[2] | r[2]))");
}

TEST_CASE("QBF Prenex K3 Edown Adown") {
  std::string_view k3_str
    = "!(# s_1_0 # s_2_0 # s_3_0 (!(? v1 ? v2 ? v3 ? v4 ? v5 "
      "((s_1_0 -> ( ((-v1 "
      "| -v4 | v5) & (v3 | -v1 | -v5))))  & (s_2_0 -> ( ((v2 "
      "| -v1 | -v3) & (v5 "
      "| -v1 | -v3))))  & (s_3_0 -> ( ((-v5 | v1 | -v4) & "
      "(-v2 | -v3 | v5))))  & "
      "1) & (# u_1_0 # u_2_0 # u_3_0 (((( s_1_0 -> u_1_0 )&( "
      "s_2_0 -> u_2_0 )&( "
      "s_3_0 -> u_3_0 )) & !(( u_1_0 -> s_1_0 )&( u_2_0 -> "
      "s_2_0 )&( u_3_0 -> "
      "s_3_0 ))) -> (# v1 # v2 # v3 # v4 # v5 ((u_1_0 -> ( "
      "((-v1 | -v4 | v5) & "
      "(v3 | -v1 | -v5))))  & (u_2_0 -> ( ((v2 | -v1 | -v3) "
      "& (v5 | -v1 | "
      "-v3))))  & (u_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 "
      "| v5)))) "
      "->!1)))))&!(# s_1_1 # s_2_1 # s_3_1 # s_4_1 (!(? v1 ? "
      "v2 ? v3 ? v4 ? v5 "
      "((s_1_1 -> ( s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 "
      "| -v5)))))  & "
      "(s_2_1 -> ( s_2_0 -> ( ((v2 | -v1 | -v3) & (v5 | -v1 "
      "| -v3)))))  & (s_3_1 "
      "-> ( s_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 | "
      "v5)))))  & (s_4_1 -> ( "
      "1))  & 5) & (# u_1_1 # u_2_1 # u_3_1 # u_4_1 (((( "
      "s_1_1 -> u_1_1 )&( "
      "s_2_1 -> u_2_1 )&( s_3_1 -> u_3_1 )&( s_4_1 -> u_4_1 "
      ")) & !(( u_1_1 -> "
      "s_1_1 )&( u_2_1 -> s_2_1 )&( u_3_1 -> s_3_1 )&( u_4_1 "
      "-> s_4_1 ))) -> (# "
      "v1 # v2 # v3 # v4 # v5 ((u_1_1 -> ( u_1_0 -> ( ((-v1 "
      "| -v4 | v5) & (v3 | "
      "-v1 | -v5)))))  & (u_2_1 -> ( u_2_0 -> ( ((v2 | -v1 | "
      "-v3) & (v5 | -v1 | "
      "-v3)))))  & (u_3_1 -> ( u_3_0 -> ( ((-v5 | v1 | -v4) "
      "& (-v2 | -v3 | "
      "v5)))))  & (u_4_1 -> ( 1)) ->!5)))))&!(# s_1_2 # "
      "s_2_2 # s_3_2 # s_4_2 # "
      "s_5_2 (!(? v1 ? v2 ? v3 ? v4 ? v5 ((s_1_2 -> ( s_1_1 "
      "-> ( s_1_0 -> ( "
      "((-v1 | -v4 | v5) & (v3 | -v1 | -v5))))))  & (s_2_2 "
      "-> ( s_2_1 -> ( s_2_0 "
      "-> ( ((v2 | -v1 | -v3) & (v5 | -v1 | -v3))))))  & "
      "(s_3_2 -> ( s_3_1 -> ( "
      "s_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 | v5))))))  "
      "& (s_4_2 -> ( s_4_1 "
      "-> ( 1)))  & (s_5_2 -> ( 5))  & 4) & (# u_1_2 # u_2_2 "
      "# u_3_2 # u_4_2 # "
      "u_5_2 (((( s_1_2 -> u_1_2 )&( s_2_2 -> u_2_2 )&( "
      "s_3_2 -> u_3_2 )&( s_4_2 "
      "-> u_4_2 )&( s_5_2 -> u_5_2 )) & !(( u_1_2 -> s_1_2 "
      ")&( u_2_2 -> s_2_2 "
      ")&( u_3_2 -> s_3_2 )&( u_4_2 -> s_4_2 )&( u_5_2 -> "
      "s_5_2 ))) -> (# v1 # "
      "v2 # v3 # v4 # v5 ((u_1_2 -> ( u_1_1 -> ( u_1_0 -> ( "
      "((-v1 | -v4 | v5) & "
      "(v3 | -v1 | -v5))))))  & (u_2_2 -> ( u_2_1 -> ( u_2_0 "
      "-> ( ((v2 | -v1 | "
      "-v3) & (v5 | -v1 | -v3))))))  & (u_3_2 -> ( u_3_1 -> "
      "( u_3_0 -> ( ((-v5 | "
      "v1 | -v4) & (-v2 | -v3 | v5))))))  & (u_4_2 -> ( "
      "u_4_1 -> ( 1)))  & "
      "(u_5_2 -> ( 5)) ->!4)))))&!(# s_1_3 # s_2_3 # s_3_3 # "
      "s_4_3 # s_5_3 # "
      "s_6_3 (!(? v1 ? v2 ? v3 ? v4 ? v5 ((s_1_3 -> ( s_1_2 "
      "-> ( s_1_1 -> ( "
      "s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 | -v5))))))) "
      " & (s_2_3 -> ( "
      "s_2_2 -> ( s_2_1 -> ( s_2_0 -> ( ((v2 | -v1 | -v3) & "
      "(v5 | -v1 | "
      "-v3)))))))  & (s_3_3 -> ( s_3_2 -> ( s_3_1 -> ( s_3_0 "
      "-> ( ((-v5 | v1 | "
      "-v4) & (-v2 | -v3 | v5)))))))  & (s_4_3 -> ( s_4_2 -> "
      "( s_4_1 -> ( 1))))  "
      "& (s_5_3 -> ( s_5_2 -> ( 5)))  & (s_6_3 -> ( 4))  & "
      "2) & (# u_1_3 # u_2_3 "
      "# u_3_3 # u_4_3 # u_5_3 # u_6_3 (((( s_1_3 -> u_1_3 "
      ")&( s_2_3 -> u_2_3 "
      ")&( s_3_3 -> u_3_3 )&( s_4_3 -> u_4_3 )&( s_5_3 -> "
      "u_5_3 )&( s_6_3 -> "
      "u_6_3 )) & !(( u_1_3 -> s_1_3 )&( u_2_3 -> s_2_3 )&( "
      "u_3_3 -> s_3_3 )&( "
      "u_4_3 -> s_4_3 )&( u_5_3 -> s_5_3 )&( u_6_3 -> s_6_3 "
      "))) -> (# v1 # v2 # "
      "v3 # v4 # v5 ((u_1_3 -> ( u_1_2 -> ( u_1_1 -> ( u_1_0 "
      "-> ( ((-v1 | -v4 | "
      "v5) & (v3 | -v1 | -v5)))))))  & (u_2_3 -> ( u_2_2 -> "
      "( u_2_1 -> ( u_2_0 "
      "-> ( ((v2 | -v1 | -v3) & (v5 | -v1 | -v3)))))))  & "
      "(u_3_3 -> ( u_3_2 -> ( "
      "u_3_1 -> ( u_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 "
      "| v5)))))))  & "
      "(u_4_3 -> ( u_4_2 -> ( u_4_1 -> ( 1))))  & (u_5_3 -> "
      "( u_5_2 -> ( 5)))  & "
      "(u_6_3 -> ( 4)) ->!2)))))&# v1 # v2 # v3 # v4 # v5 "
      "((s_1_3 -> ( s_1_2 -> "
      "( s_1_1 -> ( s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 "
      "| -v5)))))))  & "
      "(s_2_3 -> ( s_2_2 -> ( s_2_1 -> ( s_2_0 -> ( ((v2 | "
      "-v1 | -v3) & (v5 | "
      "-v1 | -v3)))))))  & (s_3_3 -> ( s_3_2 -> ( s_3_1 -> ( "
      "s_3_0 -> ( ((-v5 | "
      "v1 | -v4) & (-v2 | -v3 | v5)))))))  & (s_4_3 -> ( "
      "s_4_2 -> ( s_4_1 -> ( "
      "1))))  & (s_5_3 -> ( s_5_2 -> ( 5)))  & (s_6_3 -> ( "
      "4)) ->2)))))))))";

  isviewstream k3(k3_str);
  boole parser(k3);
  auto result = parser();
  REQUIRE(result);
  op_ref k3_root = *result;

  prenex_quantifier prenexer(prenex_quantifier::Edown_Adown);
  if(activate_animations)
    prenexer.animate("K3_Edown_Adown");
  op_ref k3_prenexed = prenexer(k3_root);

  std::string quantifiers = "?s_1_0 ?s_2_0 ?s_3_0 #s_1_1 #s_2_1 #s_3_1 #s_4_1 "
                            "?s_1_2 ?s_2_2 ?s_3_2 ?s_4_2"
                            " ?s_5_2 ?v1[1] ?v2[1] ?v3[1] ?v4[1] ?v5[1] ?v1[5] "
                            "?v2[5] ?v3[5] ?v4[5] ?v5[5]"
                            " #s_1_3 #s_2_3 #s_3_3 #s_4_3 #s_5_3 #s_6_3 #v1[7] "
                            "#v2[7] #v3[7] #v4[7] #v5[7]"
                            " #v1[3] #v2[3] #v3[3] #v4[3] #v5[3] #v1[8] #v2[8] "
                            "#v3[8] #v4[8] #v5[8] #u_1_2"
                            " #u_2_2 #u_3_2 #u_4_2 #u_5_2 #v1[4] #v2[4] #v3[4] "
                            "#v4[4] #v5[4] #u_1_0 #u_2_0"
                            " #u_3_0 #v1 #v2 #v3 #v4 #v5 ?u_1_3 ?u_2_3 ?u_3_3 "
                            "?u_4_3 ?u_5_3 ?u_6_3 ?v1[6]"
                            " ?v2[6] ?v3[6] ?v4[6] ?v5[6] ?u_1_1 ?u_2_1 ?u_3_1 "
                            "?u_4_1 ?v1[2] ?v2[2] ?v3[2]"
                            " ?v4[2] ?v5[2]";

  auto match_expression = Catch::Matchers::StartsWith(quantifiers);
  REQUIRE_THAT(k3_prenexed.to_string(), match_expression);
}

TEST_CASE("QBF Prenex K3 Eup Adown") {
  std::string_view k3_str
    = "!(# s_1_0 # s_2_0 # s_3_0 (!(? v1 ? v2 ? v3 ? v4 ? v5 "
      "((s_1_0 -> ( ((-v1 "
      "| -v4 | v5) & (v3 | -v1 | -v5))))  & (s_2_0 -> ( ((v2 "
      "| -v1 | -v3) & (v5 "
      "| -v1 | -v3))))  & (s_3_0 -> ( ((-v5 | v1 | -v4) & "
      "(-v2 | -v3 | v5))))  & "
      "1) & (# u_1_0 # u_2_0 # u_3_0 (((( s_1_0 -> u_1_0 )&( "
      "s_2_0 -> u_2_0 )&( "
      "s_3_0 -> u_3_0 )) & !(( u_1_0 -> s_1_0 )&( u_2_0 -> "
      "s_2_0 )&( u_3_0 -> "
      "s_3_0 ))) -> (# v1 # v2 # v3 # v4 # v5 ((u_1_0 -> ( "
      "((-v1 | -v4 | v5) & "
      "(v3 | -v1 | -v5))))  & (u_2_0 -> ( ((v2 | -v1 | -v3) "
      "& (v5 | -v1 | "
      "-v3))))  & (u_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 "
      "| v5)))) "
      "->!1)))))&!(# s_1_1 # s_2_1 # s_3_1 # s_4_1 (!(? v1 ? "
      "v2 ? v3 ? v4 ? v5 "
      "((s_1_1 -> ( s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 "
      "| -v5)))))  & "
      "(s_2_1 -> ( s_2_0 -> ( ((v2 | -v1 | -v3) & (v5 | -v1 "
      "| -v3)))))  & (s_3_1 "
      "-> ( s_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 | "
      "v5)))))  & (s_4_1 -> ( "
      "1))  & 5) & (# u_1_1 # u_2_1 # u_3_1 # u_4_1 (((( "
      "s_1_1 -> u_1_1 )&( "
      "s_2_1 -> u_2_1 )&( s_3_1 -> u_3_1 )&( s_4_1 -> u_4_1 "
      ")) & !(( u_1_1 -> "
      "s_1_1 )&( u_2_1 -> s_2_1 )&( u_3_1 -> s_3_1 )&( u_4_1 "
      "-> s_4_1 ))) -> (# "
      "v1 # v2 # v3 # v4 # v5 ((u_1_1 -> ( u_1_0 -> ( ((-v1 "
      "| -v4 | v5) & (v3 | "
      "-v1 | -v5)))))  & (u_2_1 -> ( u_2_0 -> ( ((v2 | -v1 | "
      "-v3) & (v5 | -v1 | "
      "-v3)))))  & (u_3_1 -> ( u_3_0 -> ( ((-v5 | v1 | -v4) "
      "& (-v2 | -v3 | "
      "v5)))))  & (u_4_1 -> ( 1)) ->!5)))))&!(# s_1_2 # "
      "s_2_2 # s_3_2 # s_4_2 # "
      "s_5_2 (!(? v1 ? v2 ? v3 ? v4 ? v5 ((s_1_2 -> ( s_1_1 "
      "-> ( s_1_0 -> ( "
      "((-v1 | -v4 | v5) & (v3 | -v1 | -v5))))))  & (s_2_2 "
      "-> ( s_2_1 -> ( s_2_0 "
      "-> ( ((v2 | -v1 | -v3) & (v5 | -v1 | -v3))))))  & "
      "(s_3_2 -> ( s_3_1 -> ( "
      "s_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 | v5))))))  "
      "& (s_4_2 -> ( s_4_1 "
      "-> ( 1)))  & (s_5_2 -> ( 5))  & 4) & (# u_1_2 # u_2_2 "
      "# u_3_2 # u_4_2 # "
      "u_5_2 (((( s_1_2 -> u_1_2 )&( s_2_2 -> u_2_2 )&( "
      "s_3_2 -> u_3_2 )&( s_4_2 "
      "-> u_4_2 )&( s_5_2 -> u_5_2 )) & !(( u_1_2 -> s_1_2 "
      ")&( u_2_2 -> s_2_2 "
      ")&( u_3_2 -> s_3_2 )&( u_4_2 -> s_4_2 )&( u_5_2 -> "
      "s_5_2 ))) -> (# v1 # "
      "v2 # v3 # v4 # v5 ((u_1_2 -> ( u_1_1 -> ( u_1_0 -> ( "
      "((-v1 | -v4 | v5) & "
      "(v3 | -v1 | -v5))))))  & (u_2_2 -> ( u_2_1 -> ( u_2_0 "
      "-> ( ((v2 | -v1 | "
      "-v3) & (v5 | -v1 | -v3))))))  & (u_3_2 -> ( u_3_1 -> "
      "( u_3_0 -> ( ((-v5 | "
      "v1 | -v4) & (-v2 | -v3 | v5))))))  & (u_4_2 -> ( "
      "u_4_1 -> ( 1)))  & "
      "(u_5_2 -> ( 5)) ->!4)))))&!(# s_1_3 # s_2_3 # s_3_3 # "
      "s_4_3 # s_5_3 # "
      "s_6_3 (!(? v1 ? v2 ? v3 ? v4 ? v5 ((s_1_3 -> ( s_1_2 "
      "-> ( s_1_1 -> ( "
      "s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 | -v5))))))) "
      " & (s_2_3 -> ( "
      "s_2_2 -> ( s_2_1 -> ( s_2_0 -> ( ((v2 | -v1 | -v3) & "
      "(v5 | -v1 | "
      "-v3)))))))  & (s_3_3 -> ( s_3_2 -> ( s_3_1 -> ( s_3_0 "
      "-> ( ((-v5 | v1 | "
      "-v4) & (-v2 | -v3 | v5)))))))  & (s_4_3 -> ( s_4_2 -> "
      "( s_4_1 -> ( 1))))  "
      "& (s_5_3 -> ( s_5_2 -> ( 5)))  & (s_6_3 -> ( 4))  & "
      "2) & (# u_1_3 # u_2_3 "
      "# u_3_3 # u_4_3 # u_5_3 # u_6_3 (((( s_1_3 -> u_1_3 "
      ")&( s_2_3 -> u_2_3 "
      ")&( s_3_3 -> u_3_3 )&( s_4_3 -> u_4_3 )&( s_5_3 -> "
      "u_5_3 )&( s_6_3 -> "
      "u_6_3 )) & !(( u_1_3 -> s_1_3 )&( u_2_3 -> s_2_3 )&( "
      "u_3_3 -> s_3_3 )&( "
      "u_4_3 -> s_4_3 )&( u_5_3 -> s_5_3 )&( u_6_3 -> s_6_3 "
      "))) -> (# v1 # v2 # "
      "v3 # v4 # v5 ((u_1_3 -> ( u_1_2 -> ( u_1_1 -> ( u_1_0 "
      "-> ( ((-v1 | -v4 | "
      "v5) & (v3 | -v1 | -v5)))))))  & (u_2_3 -> ( u_2_2 -> "
      "( u_2_1 -> ( u_2_0 "
      "-> ( ((v2 | -v1 | -v3) & (v5 | -v1 | -v3)))))))  & "
      "(u_3_3 -> ( u_3_2 -> ( "
      "u_3_1 -> ( u_3_0 -> ( ((-v5 | v1 | -v4) & (-v2 | -v3 "
      "| v5)))))))  & "
      "(u_4_3 -> ( u_4_2 -> ( u_4_1 -> ( 1))))  & (u_5_3 -> "
      "( u_5_2 -> ( 5)))  & "
      "(u_6_3 -> ( 4)) ->!2)))))&# v1 # v2 # v3 # v4 # v5 "
      "((s_1_3 -> ( s_1_2 -> "
      "( s_1_1 -> ( s_1_0 -> ( ((-v1 | -v4 | v5) & (v3 | -v1 "
      "| -v5)))))))  & "
      "(s_2_3 -> ( s_2_2 -> ( s_2_1 -> ( s_2_0 -> ( ((v2 | "
      "-v1 | -v3) & (v5 | "
      "-v1 | -v3)))))))  & (s_3_3 -> ( s_3_2 -> ( s_3_1 -> ( "
      "s_3_0 -> ( ((-v5 | "
      "v1 | -v4) & (-v2 | -v3 | v5)))))))  & (s_4_3 -> ( "
      "s_4_2 -> ( s_4_1 -> ( "
      "1))))  & (s_5_3 -> ( s_5_2 -> ( 5)))  & (s_6_3 -> ( "
      "4)) ->2)))))))))";

  isviewstream k3(k3_str);
  boole parser(k3);
  auto result = parser();
  REQUIRE(result);
  op_ref k3_root = *result;

  prenex_quantifier prenexer(prenex_quantifier::Eup_Adown);
  op_ref k3_prenexed = prenexer(k3_root);

  std::string quantifiers = "?s_1_0 ?s_2_0 ?s_3_0 ?v1[1] ?v2[1] ?v3[1] ?v4[1] "
                            "?v5[1] #s_1_1 #s_2_1 #s_3_1"
                            " #s_4_1 ?s_1_2 ?s_2_2 ?s_3_2 ?s_4_2 ?s_5_2 ?v1[5] "
                            "?v2[5] ?v3[5] ?v4[5] ?v5[5]"
                            " #s_1_3 #s_2_3 #s_3_3 #s_4_3 #s_5_3 #s_6_3 #v1[7] "
                            "#v2[7] #v3[7] #v4[7] #v5[7]"
                            " #u_1_0 #u_2_0 #u_3_0 #v1 #v2 #v3 #v4 #v5 #v1[3] "
                            "#v2[3] #v3[3] #v4[3] #v5[3]"
                            " #u_1_2 #u_2_2 #u_3_2 #u_4_2 #u_5_2 #v1[4] #v2[4] "
                            "#v3[4] #v4[4] #v5[4] #v1[8]"
                            " #v2[8] #v3[8] #v4[8] #v5[8] ?u_1_3 ?u_1_1 ?u_2_1 "
                            "?u_3_1 ?u_4_1 ?v1[2] ?v2[2]"
                            " ?v3[2] ?v4[2] ?v5[2] ?u_2_3 ?u_3_3 ?u_4_3 ?u_5_3 "
                            "?u_6_3 ?v1[6] ?v2[6] ?v3[6]"
                            " ?v4[6] ?v5[6]";

  auto match_expression = Catch::Matchers::StartsWith(quantifiers);
  REQUIRE_THAT(k3_prenexed.to_string(), match_expression);
}

TEST_CASE("QBF prenex with duplicated structure", "[prenexer]") {
  std::string_view in_str
    = "(((#a ?b (a | b)) | !(#a ?b (a | b))) & ((#a ?b (a "
      "| b)) | !(#a ?b (a | b))))";

  isviewstream in(in_str);
  boole parser(in);
  auto result = parser();
  REQUIRE(result);
  op_ref root = *result;

  REQUIRE(!root->is_prenex);

  prenex_quantifier prenexer(prenex_quantifier::Eup_Aup);
  op_ref prenexed = prenexer(root);

  REQUIRE(prenexed->is_prenex);
}

TEST_CASE("QBF prenex on fuzzed input", "[prenexer]") {
  std::string_view in_str = "((?2 (1)) | !(?2 (1))) & ((?2 (1)) | !(?2 (1)))";

  isviewstream in(in_str);
  boole parser(in);
  auto result = parser();
  REQUIRE(result);
  op_ref root = *result;

  REQUIRE(!root->is_prenex);

  prenex_quantifier prenexer(prenex_quantifier::Eup_Aup);
  op_ref prenexed = prenexer(root);

  REQUIRE(prenexed->is_prenex);
}
