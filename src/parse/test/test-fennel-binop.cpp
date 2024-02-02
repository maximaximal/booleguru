#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

#include <boole_lexer.h>
#include <boole_parser.h>

using namespace booleguru::parse;
using namespace booleguru::expression;

TEST_CASE("Use a binop implemented in fennel", "[fennel_binop]") {
  std::string_view input = "(#a ?b a^b) ::model-equivalent@a@b (#a ?b a <-> !b)";
  auto is = isviewstream(input);
  boole parser(is);
  parser.eval(true);
  auto res = parser();

  if(!res) {
    CAPTURE(res.message);
    REQUIRE(false);
  }
  REQUIRE(res);
}
