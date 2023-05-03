#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/quantvec.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Mark leaves in a quantvec") {
  quantvec v;
  v.add(op_type::Forall, 0, 0);
  v.add(op_type::Exists, 1, 1);
  v.add(op_type::Forall, 2, 2);
  v.add(op_type::Forall, 3, 2);
  v.add(op_type::Forall, 4, 2);
  v.add(op_type::Forall, 5, 1);
  v.mark_leaves();

  REQUIRE(!v.is_leaf(0));
  REQUIRE(!v.is_leaf(1));
  REQUIRE(v.is_leaf(2));
  REQUIRE(v.is_leaf(3));
  REQUIRE(v.is_leaf(4));
  REQUIRE(v.is_leaf(5));
}
