#include <array>
#include <iostream>
#include <random>

#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/mutator.hpp>

using namespace booleguru::expression;

TEST_CASE("Mutate a vector of ops", "[mutator]") {
  std::minstd_rand r(1331);
  mutator m(r);

  std::array<op, 100> ops;
  size_t size = 0;

  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());
  m.mutate(ops.data(), size, ops.size());

  /*
  for(size_t i = 0; i < size; ++i) {
    std::cout << i << " -- " << ops[i] << std::endl;
  }
   */

  REQUIRE(size > 0);
  REQUIRE((op_type)ops[0].type != op_type::None);
}
