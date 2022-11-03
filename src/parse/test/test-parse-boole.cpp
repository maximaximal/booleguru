#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

#include <iostream>

using namespace booleguru::parse;

TEST_CASE("Parse example boole formulas and check the resulting expressions") {
  std::string_view input = GENERATE("(a & b)",
                                    "(a | b)",
                                    "(a | a | b)",
                                    "(a | a | b | b)",
                                    "(a & (a -> b))",
                                    "(a & b & c)",
                                    "(!a & b & c)",
                                    "(!a & !b & c)",
                                    "(!a & !b & !c)",
                                    "(a & !b & !c)",
                                    "(a & b & !c)",
                                    "((a | b) & !c)",
                                    "(!(a | b) & !c)",
                                    "((a & (p | q | e)) <-> c)",
                                    "a <- b",
                                    "a <-> b",
                                    "a<->b",
                                    "a<-b",
                                    "a_b<-b",
                                    u8"ä&ö",
                                    u8"a∧b",
                                    u8"ä∧ö",
                                    u8"?a @ö a∧ö",
                                    u8"?a@ö a∧ö",
                                    u8"?a ∃ö a∧ö",
                                    "(a) <-> (ll -> b)");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::stringstream s;
  s << *res;
  std::string inputs(input);
  std::string stringified = s.str();

  CAPTURE(inputs);
  CAPTURE(*res);
  REQUIRE(res);
}

TEST_CASE("Parse invalid boolean formulas") {
  std::string_view input = GENERATE("a b a");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  REQUIRE(!res);
}

TEST_CASE("Parse formula containing lisp code") {
  // Interesting case: "(list 'nil) a"
  // Currently, the parser does not support first executing lisp, then
  // continuing with the rest of the formula!

  std::string_view input = GENERATE("a (list 'nil)");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  REQUIRE(res);
}
