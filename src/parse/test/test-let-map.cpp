#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/let_map.hpp>

using namespace booleguru::parse;

TEST_CASE("Manipulate and query a let_map object", "[let_map]") {
  let_map l;
  l.push();
  l.add("1", 2);
  l.add("2", 3);
  REQUIRE(l["1"] == 2);
  REQUIRE(l["2"] == 3);
  l.pop();

  REQUIRE(l["1"] == 0);

  l.push();
  REQUIRE(l["1"] == 0);
  l.add("1", 3);
  REQUIRE(l["1"] == 3);
  l.push();
  l.add("1", 4);
  REQUIRE(l["1"] == 4);
  l.pop();
  REQUIRE(l["1"] == 3);
  l.pop();

  REQUIRE(l["1"] == 0);
}
