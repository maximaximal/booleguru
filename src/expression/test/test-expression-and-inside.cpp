#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

using namespace booleguru::expression;

TEST_CASE("Test creating op objects results in and_inside correctly") {
  op o(op_type::And, 1, 2);
  REQUIRE(o.and_inside);

  op other(op_type::Or, 1, 2);
  REQUIRE(!other.and_inside);
}

TEST_CASE(
  "Test creating op objects via the op manager and check for and_inside") {
  op_manager ops;
  auto v = ops.vars().get(variable{ "Test" });
  auto v1 = ops.get(op(op_type::Var, v.get_id(), 0));
  auto v2 = ops.get(op(op_type::Var, v.get_id(), 0));
  auto a = v1 && v2;
  REQUIRE(a->and_inside);
}