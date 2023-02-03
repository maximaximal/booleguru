#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/var_manager.hpp>
#include <booleguru/expression/literals.hpp>

#include <booleguru/transform/eliminate_implication.hpp>
#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/cnf.hpp>

using namespace booleguru::expression;
using namespace booleguru::transform;
using namespace booleguru::expression::literals;

TEST_CASE("Test resolving simple equivalence") {
  op_manager ops;

  op_ref x = "x"_var(ops);
  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);


    auto formula = equi(x, a && b && c);

    auto equiv_ = distribute_to_cnf(formula);

   const char* expected = "(x | !a | !b | !c) & (!x | a) & (!x | b) & (!x | c)";

  REQUIRE(equiv_.to_string() == expected);
}

TEST_CASE("Test resolving equivalence") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  op_ref d = "d"_var(ops);
  op_ref e = "e"_var(ops);



    auto formula = equi((a && b), c && d && e);

    auto equiv_ = distribute_to_cnf(formula);

    //CAPTURE(formula);
    //REQUIRE(false);

   const char* expected = "(a | !c | !d | !e) & (b | !c | !d | !e) & (!a | !b | c) & (!a | !b | d) & (!a | !b | e)";

  REQUIRE(equiv_.to_string() == expected);
}

TEST_CASE("Test resolving equivalence and implications with unoptimised output") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  
    auto formula = equi(impl(p, q), impl(p, r));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(!p | q | p) & (!p | q | !r) & (p | !p | r) & (!q | !p | r)";

  REQUIRE(equiv_.to_string() == expected);
}

TEST_CASE("Test resolving equivalence and implications with optimised output", "[.]") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  
    auto formula = equi(impl(p, q), impl(p, r));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(!p | q | !r) & (!q | !p | r)";

  REQUIRE(equiv_.to_string() == expected);
}


TEST_CASE("Test resolving equivalence and implication and others") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  op_ref d = "d"_var(ops);
  op_ref e = "e"_var(ops);
  
    auto formula_ = !(equi(a, b));
    auto formula = impl(formula_, (!(c && d) && e));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(a | !b | !c | !d) & (a | !b | e) & (!a | b | !c | !d) & (!a | b | e)";

  REQUIRE(equiv_.to_string() == expected);
}

TEST_CASE("Simple Test 1") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  
    auto formula = impl(impl(a, b), c);

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(a | c) & (!b | c)";

  REQUIRE(equiv_.to_string() == expected);
}
TEST_CASE("Simple Test 2") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  
    auto formula = impl(a, impl(b, c));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "!a | !b | c";

  REQUIRE(equiv_.to_string() == expected);
}

TEST_CASE("Simple Test 3") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  
    auto formula = impl(a, b) || impl(b, a);

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "!a | b | !b | a";

  REQUIRE(equiv_.to_string() == expected);
}
TEST_CASE("Simple Test 4") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  
    auto formula = impl(!p, impl(p, q));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "p | !p | q";

  REQUIRE(equiv_.to_string() == expected);
}
TEST_CASE("Simple Test 5") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  
    auto formula_1 = impl(q, r);
    auto formula_2 = impl(r, q);
    auto formula = impl(impl(p, formula_1), impl(p, formula_2));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(p | !p | !r | q) & (q | !p | !r | q) & (!r | !p | !r | q)";

  REQUIRE(equiv_.to_string() == expected);
}
TEST_CASE("Simple Test 6") {
  op_manager ops;

  op_ref p = "p"_var(ops);
  op_ref q = "q"_var(ops);
  op_ref r = "r"_var(ops);
  
    auto formula_1 = impl(q, r);
    auto formula_2 = impl(p, r);
    auto formula_3 = impl(p, q);
    auto formula = impl(formula_3, impl(formula_1, formula_2));

    auto equiv_ = distribute_to_cnf(formula);

    CAPTURE(formula);

   const char* expected = "(p | !p | q | r) & (!p | q | !q | r)";

  REQUIRE(equiv_.to_string() == expected);
}