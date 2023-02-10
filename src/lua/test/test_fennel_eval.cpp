#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/op_manager.hpp>

#include <booleguru/lua/lua-context.hpp>

using namespace booleguru::lua;

TEST_CASE("Evaluate simple fennel code") {
  lua_context ctx;
  auto ret = ctx.eval_fennel("(+ 1 1)");
  REQUIRE(ret.index() == 1);
  REQUIRE(std::holds_alternative<long int>(ret));
  REQUIRE(std::get<long int>(ret) == 2);
}
