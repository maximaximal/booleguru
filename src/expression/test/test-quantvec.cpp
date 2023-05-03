#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/quantvec.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Mark leaves in a quantvec and extract the critical path") {
  quantvec v;
  v.add(op_type::Forall, 0, 0);
  v.add(op_type::Exists, 1, 1);
  v.add(op_type::Forall, 2, 2);
  v.add(op_type::Exists, 3, 2);
  v.add(op_type::Exists, 4, 2);
  v.add(op_type::Forall, 5, 1);
  v.mark_leaves();

  REQUIRE(!v.is_leaf(0));
  REQUIRE(!v.is_leaf(1));
  REQUIRE(v.is_leaf(2));
  REQUIRE(v.is_leaf(3));
  REQUIRE(v.is_leaf(4));
  REQUIRE(v.is_leaf(5));

  quantvec c{ v.extract_critical_path(true) };

  REQUIRE(c.size() == 3);
  REQUIRE(c.type(0) == op_type::Forall);
  REQUIRE(c.type(1) == op_type::Exists);
  REQUIRE(c.type(2) == op_type::Forall);

  // When keeping the critical path, the size should stay at 6. Otherwise, the
  // elements are removed.
  REQUIRE(v.size() == 6);
  v.extract_critical_path();
  REQUIRE(v.size() == 3);

  REQUIRE(v.type(0) == op_type::Exists);
  REQUIRE(v.type(1) == op_type::Exists);
  REQUIRE(v.type(2) == op_type::Forall);
}

TEST_CASE(
  "Extract the critical path from a quanttree against other sub-trees") {
  quantvec v;
  v.add(op_type::Forall, 0 /* var */, 0 /* nesting */);
  v.add(op_type::Exists, 1, 1);
  v.add(op_type::Forall, 2, 2);
  v.add(op_type::Exists, 2, 3);
  v.add(op_type::Exists, 3, 2);
  v.add(op_type::Forall, 3, 3);
  v.add(op_type::Exists, 3, 4);
  v.add(op_type::Exists, 4, 2);
  v.add(op_type::Forall, 5, 1);

  quantvec c{ v.extract_critical_path() };

  REQUIRE(c.size() == 5);
  REQUIRE(c.type(0) == op_type::Forall);
  REQUIRE(c.type(1) == op_type::Exists);
  REQUIRE(c.type(2) == op_type::Exists);
  REQUIRE(c.type(3) == op_type::Forall);
  REQUIRE(c.type(4) == op_type::Exists);
}

TEST_CASE("Insert quantifiers into with flipping contexts") {
  quantvec v;
  v.add(op_type::Forall, 0, 0);
  {
    quantvec::flip_ctx ctx{ v.open_flip_ctx() };
    v.add(op_type::Exists, 0, 0);

    quantvec::flip_ctx ctx_{ v.open_flip_ctx() };
    v.add(op_type::Forall, 0, 0);
  }
  v.add(op_type::Exists, 0, 0);

  REQUIRE(v.type(0) == op_type::Forall);
  REQUIRE(v.type(1) == op_type::Forall);
  REQUIRE(v.type(2) == op_type::Forall);
  REQUIRE(v.type(3) == op_type::Exists);
}
