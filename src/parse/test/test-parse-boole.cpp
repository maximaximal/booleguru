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
                                    "a <-> (ll -> b)");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::stringstream s;
  if(res)
    s << *res;
  else
    s << "((#no-expr#))";
  std::string inputs(input);
  std::string stringified = s.str();

  CAPTURE(inputs);
  if(res) {
    CAPTURE(*res);
  }
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
  // TODO:
  // Interesting case: "(list 'nil) a"
  // Currently, the parser does not support first executing lisp, then
  // continuing with the rest of the formula!
  //
  // Another case that's still to do: "a & (list 'nil)". This loops in
  // parse_assoc_op<and>.

  std::string_view input = GENERATE(//"a (list 'nil)",
                                    //"a (print (var 'test))",
                                    "(var 'a) & a"
                                    );
                                    //"(print (values 'demo 'demo)) a");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  if(res) {
    CAPTURE(*res);
    REQUIRE(false);
  }
  REQUIRE(res);
}
