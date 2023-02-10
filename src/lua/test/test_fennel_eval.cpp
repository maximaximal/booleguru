#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/op_manager.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <iostream>

using namespace booleguru::lua;
using namespace booleguru::expression;

TEST_CASE("Evaluate simple fennel code") {
  lua_context ctx;
  auto ret = ctx.eval_fennel("(+ 1 1)");
  REQUIRE(ret.index() == 1);
  REQUIRE(std::holds_alternative<long int>(ret));
  REQUIRE(std::get<long int>(ret) == 2);
}

TEST_CASE("Build a simple formula through fennel") {
  lua_context ctx;
  auto ret = ctx.eval_fennel("(b-and (b-var \"a\") (b-var \"b\"))");
  REQUIRE(std::holds_alternative<op_ref>(ret));
  op_ref expr = std::get<op_ref>(ret);
  REQUIRE(expr.to_string() == "a & b");
}
