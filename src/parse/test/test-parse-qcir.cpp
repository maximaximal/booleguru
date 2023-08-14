#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/qcir.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression::literals;
using booleguru::expression::op_manager;

static const std::string_view test1 = R"(#QCIR-G14
forall(a, b)
exists(c)
output(g2)
g1 = and(a, b)
g2 = or(g1, c)
)";

static const std::string_view test2 = R"(# Just some comment..?
#QCIR-14 23
output(g)
g = or()
)";

static const std::string_view test3 = R"(# Just some comment..?
#  QCIR-13
 # More comments
# More
output( g )
g=and()
)";

TEST_CASE("Parse example QCIR formulas", "[parser][qcir]") {
  const std::string_view input = GENERATE(test1, test2, test3);
  auto is = isviewstream(input);
  qcir parser(is);
  auto res = parser();

  CAPTURE(input);
  if(res) {
    CAPTURE(*res);
  }
  CAPTURE(res.message);
  REQUIRE(res);

  // printf("%s\n", stringified.c_str());
}

TEST_CASE("Parse and check small QCIR formulas", "[parser][qcir]") {
  auto is = isviewstream(test1);
  std::shared_ptr<op_manager> ops = std::make_shared<op_manager>();
  qcir parser(is, ops);
  auto res = parser();
  REQUIRE(res);

  auto g2_res = *res;

  // Build the same formula manually afterwards.
  auto a = "a"_var(ops);
  auto b = "b"_var(ops);
  auto c = "c"_var(ops);
  auto g1 = a && b;
  auto g2 = g1 || c;
  auto quantified_g2 = forall(a, forall(b, exists(c, g2)));

  REQUIRE(g2_res == quantified_g2);
}
