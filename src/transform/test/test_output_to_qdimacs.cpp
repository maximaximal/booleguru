#include <sstream>

#include <catch2/catch_test_macros.hpp>

#include <booleguru/transform/output_to_qdimacs.hpp>

using namespace booleguru::transform;

TEST_CASE("Output a simple CNF formula to QDIMACS format") {
  std::stringstream o;
  output_to_qdimacs output(o);
  output.problem(3, 3);
  output.end_prefix();
  output.unit(1);
  output.binary(-1, 2);
  output.ternary(2, 1, -3);
  REQUIRE(o.str() == "p cnf 3 3\n1 0\n-1 2 0\n2 1 -3 0\n");
}

TEST_CASE("Output a simple quantified CNF formula to QDIMACS format") {
  std::stringstream o;
  output_to_qdimacs output(o);
  output.problem(3, 3);
  output.exists(1);
  output.exists(2);
  output.forall(3);
  output.end_prefix();
  output.unit(1);
  output.binary(-1, 2);
  output.ternary(2, 1, -3);
  REQUIRE(o.str() == "p cnf 3 3\ne 1 2 0\na 3 0\n1 0\n-1 2 0\n2 1 -3 0\n");
}
