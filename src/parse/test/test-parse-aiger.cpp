#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/aiger.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression::literals;

static const std::string_view test_and = R"(aag 3 2 0 1 1
2
4
6
6 2 4
i0 x
i1 y
o0 o
)";

TEST_CASE("Parse simple aiger files with AND gates and symbols", "[aiger]") {
  auto is = isviewstream(test_and);
  aiger parser(is);
  auto res = parser();

  CAPTURE(res.message);

  REQUIRE(res);
}
