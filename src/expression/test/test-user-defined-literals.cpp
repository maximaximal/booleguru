#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Create a variable using a literal without a handle") {
  var_ref v = "test"_varref;
  REQUIRE(v.valid());
}

TEST_CASE("Create a variable using a literal with a handle") {
  var_manager vars;
  var_ref v = "test"_varref(vars);
  REQUIRE(v.valid());
}

TEST_CASE("Create a variable OP using a literal without a handle") {
  op_ref v = "test2"_var;
  REQUIRE(v.valid());
}

TEST_CASE("Create a variable OP using a literal with a handle") {
  op_manager ops;
  op_ref v = "test2"_var(ops);
  REQUIRE(v.valid());
}

TEST_CASE("Create quantifier using short-hand syntax") {
  op_manager ops;
  op_ref x = "x"_var(ops);
  op_ref y = "y"_var(ops);

  REQUIRE(x.get_id() == 1);
  REQUIRE(x->var.v == 1);
  REQUIRE(y.get_id() == 2);
  REQUIRE(y->var.v == 2);

  auto formula = forall(x, exists(y, x && y));

  CAPTURE(formula);

  REQUIRE(formula.to_string() == "#x ?y (x & y)");
}
