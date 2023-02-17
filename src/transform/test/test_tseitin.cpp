#include <sstream>

#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/transform/output_to_qdimacs.hpp>
#include <booleguru/transform/output_to_op.hpp>
#include <booleguru/transform/tseitin.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;
using namespace booleguru::transform;

TEST_CASE("Tseitin-transform a simple formula to QDIMACS") {
  op_manager ops;
  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  op_ref formula = exists(a, forall(b, exists(c, (a && b) || c)));

  REQUIRE(formula->is_prenex);
  REQUIRE(!formula->is_cnf);

  std::stringstream output;
  tseitin<output_to_qdimacs> tseitin_transformer(output);
  tseitin_transformer(formula);

  const char* expected = R"(p cnf 5 7
e 1 0
a 2 0
e 3 4 5 0
-1 -2 4 0
1 -4 0
2 -4 0
4 3 -5 0
-4 5 0
-3 5 0
5 0
)";

  REQUIRE(output.str() == expected);
}

TEST_CASE("Tseitin-transform a simple formula to an op tree") {
  op_manager ops;
  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  op_ref formula = exists(a, forall(b, exists(c, (a && b) || c)));

  REQUIRE(formula->is_prenex);
  REQUIRE(!formula->is_cnf);

  tseitin<output_to_op> tseitin_transformer(ops);
  op_ref result = tseitin_transformer(formula);

  CAPTURE(result.to_string());
  REQUIRE(result->is_cnf);
}
