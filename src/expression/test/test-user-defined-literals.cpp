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
  op_manager vars;
  op_ref v = "test2"_var(vars);
  REQUIRE(v.valid());
}
