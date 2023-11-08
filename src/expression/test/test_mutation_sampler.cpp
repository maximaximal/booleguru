#include <random>

#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/mutation.hpp>
#include <booleguru/expression/mutation_sampler.hpp>

using namespace booleguru::expression;

TEST_CASE("Sample from a few different mutations and select one", "[mutator]") {
  std::minstd_rand r(42);
  mutation_sampler s(r);
  s.try_mutation(3, mutation::change, op(op_type::And, 0, 0));
  s.try_mutation(4, mutation::change, op(op_type::And, 1, 0));
  s.try_mutation(5, mutation::change, op(op_type::And, 2, 0));
  s.try_mutation(6, mutation::change, op(op_type::And, 3, 0));

  CAPTURE(s.selected());

  REQUIRE(s.selected().m == mutation::change);
  REQUIRE(s.selected().o.type == op_type::And);
}
