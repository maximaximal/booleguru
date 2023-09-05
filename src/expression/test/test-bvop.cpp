#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/bvop_manager.hpp>

using namespace booleguru::expression;
using enum bvop_type;

TEST_CASE("Initiate a small bitvector DAG") {
  bvop_manager mgr;
  auto bv1 = mgr.get(bvop(bv, 2, 3));
  auto bv2 = mgr.get(bvop(bv, 2, 4));
  auto b = mgr.get(bvop(bvadd, bv1.get_id(), bv2.get_id()));

  REQUIRE(b->left() == bv1.get_id());
  REQUIRE(b->right() == bv2.get_id());

  REQUIRE(b.left()->var() == 3);
  REQUIRE(b.right()->var() == 4);
}
