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
    R"(?p[14] ?r[12] #q[11] #q[10] ?r[9] ((p[14] | q[10] | r[9]) & (!p[14] | q[11] | r[12])))";

  REQUIRE(transformed.str() == expected);
}

TEST_CASE("Transform a simple Non-Prenex cleansed formula into prenex formula "
          "(check negation of quantifiers") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

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

  std::stringstream serialized;
  serialized << op_g14;

  CAPTURE(serialized.str());

  prenex_quantifier p;
  p(op_g14);

  std::stringstream transformed;
  transformed << p(op_g14);

  CAPTURE(transformed.str());

  const char* expected = R"(
    ?p #q ?r #s ?t , ?p #q' ?r', ?p ?q'' #r''
    ((p | q | r | s | t) & (p | q' | r') & !(p | q'' | r''))
)";

  REQUIRE(transformed.str() == expected);
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
