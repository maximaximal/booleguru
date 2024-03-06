#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/transform/polarity_extractor.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;
using namespace booleguru::transform;

TEST_CASE("Extract polarities of a simple expression 1") {
  op_manager ops;
  auto a = "a"_var(ops);
  auto b = "b"_var(ops);

  auto comb = a && b;
  
  auto f = comb || !comb;
  
  polarity_extractor ex;

  a->user_flag4 = true;

  ex.reset_user_4_5_mark(f);

  REQUIRE_FALSE(a->user_flag4);
  REQUIRE_FALSE(a->user_flag5);
  REQUIRE_FALSE(comb->user_flag4);
  REQUIRE_FALSE(comb->user_flag5);

  ex(f);

  REQUIRE(a->user_flag4);
  REQUIRE(a->user_flag5);

  REQUIRE(b->user_flag4);
  REQUIRE(b->user_flag5);

  REQUIRE(comb->user_flag4);
  REQUIRE(comb->user_flag5);
}

TEST_CASE("Extract polarities of a simple expression 2") {
  op_manager ops;
  auto a = "a"_var(ops);
  auto b = "b"_var(ops);

  auto comb1 = !a && b;
  auto comb2 = a && !b;
  auto f = comb1 && !comb2;
  
  polarity_extractor ex;

  ex.reset_user_4_5_mark(f);

  ex(f);

  REQUIRE_FALSE(a->user_flag4);
  REQUIRE(a->user_flag5);

  REQUIRE(b->user_flag4);
  REQUIRE_FALSE(b->user_flag5);

  REQUIRE(f->user_flag4);
  REQUIRE_FALSE(f->user_flag5);
}
