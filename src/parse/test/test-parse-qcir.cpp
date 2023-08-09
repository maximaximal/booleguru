#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/qcir.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;

static const std::string_view test1 = R"(#QCIR-G14
forall(a, b)
exists(c)
output(g2)
g1 = and(a, b)
g2 = or(g1, c)
)";

static const std::string_view test2 = R"(# Just some comment..?
#QCIR-G14 23
output(g)
g = or()
)";

static const std::string_view test3 = R"(# Just some comment..?
#  QCIR-G13
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

  std::string inputs(input);
  std::string stringified = res->to_string();

  CAPTURE(inputs);
  if(res) {
    CAPTURE(*res);
  }
  CAPTURE(res.message);
  REQUIRE(res);

  // printf("%s\n", stringified.c_str());
}
