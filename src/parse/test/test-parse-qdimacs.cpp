#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/qdimacs.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression;

static std::string_view qdimacs_test_1 = R"(c comment 1
c comment 2
p cnf 2 2
e 1 0
a 2 0
-1 2 0
2 -1 0
)";

TEST_CASE(
  "Parse easy example qdimacs formula and check the resulting expression") {

  // This test is extremely basic and just serves to cement QDIMACS support into
  // the test suite for now.

  auto is = isviewstream(qdimacs_test_1);
  qdimacs p(is);
  auto res = p();

  REQUIRE(res);

  REQUIRE(res->to_string() == "?1 #2 ((!1 | 2) & (2 | !1))");
}
