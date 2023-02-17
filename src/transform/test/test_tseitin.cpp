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

  CAPTURE(output.str());
  REQUIRE(false);
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
  REQUIRE(false);
}
