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
  std::string_view input = GENERATE("a b a", "a & :(b)", "a :( b", "a : b");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  REQUIRE(!res);
}

TEST_CASE("Parse formula containing lisp code") {
  std::string_view input = GENERATE(
    "a & (var \"b\")",
    "(var \"a\") & b",
    "(a & (var \"b\"))",
    "(var \"a\") & (var \"b\")",
    "(b-and (var \"a\") (var \"b\"))",
    "(b-and (var \"a\") (var \"b\")) :(b-and (var \"a\") (var \"b\"))",
    "(b-and (var \"a\") (var \"b\")) :(b-and (var \"aa\") (var "
    "\"bb\")) :(b-and (var \"a\") (var \"b\"))",
    "(b-and (var \"a\") (var \"b_\")) :(b-var-rename *last-op* \"b_\" \"b\")");
  auto is = isviewstream(input);
  boole parser(is);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  if(res) {
    std::stringstream f;
    f << *res;
    REQUIRE(f.str() == "a & b");
  }
  REQUIRE(res);
}
