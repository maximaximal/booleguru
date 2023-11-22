#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/bvop_manager.hpp>

using namespace booleguru::expression;
using enum bvop_type;

TEST_CASE("Initiate a small bitvector DAG") {
  bvop_manager mgr;
  auto bv1 = mgr.get(bvop(bvvar, var_id(2), static_cast<uint16_t>(3)));
  auto bv2 = mgr.get(bvop(bvvar, var_id(3), static_cast<uint16_t>(4)));
  auto b = mgr.get(bvop(bvadd, bv1.get_id(), bv2.get_id()));

  REQUIRE(b->left() == bv1.get_id());
  REQUIRE(b->right() == bv2.get_id());

  REQUIRE(b.left()->varop.v.id_ == 2);
  REQUIRE(b.right()->varop.v.id_ == 3);
}
