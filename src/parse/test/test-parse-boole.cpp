#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression;

TEST_CASE("Parse example boole formulas and check the resulting expressions") {
  std::string_view input = GENERATE("(a & b)",
                                    "(a | b)",
                                    "(a | a | b)",
                                    "(a | a | b | b)",
                                    "(a & (a -> b))",
                                    "(a & b & c)",
                                    "(a & b ^ c)",
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
                                    "?a #o a|o",
                                    u8"?a #o a∧o",
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

  std::string inputs(input);
  std::string stringified = res->to_string();

  CAPTURE(inputs);
  if(res) {
    CAPTURE(*res);
  }
  CAPTURE(res.message);
  REQUIRE(res);
}

TEST_CASE("Parse invalid boolean formulas") {
  std::string_view input = GENERATE("a b a", "a & :(b)", "a :( b", "a : b");
  auto is = isviewstream(input);
  boole parser(is);
  parser.eval(true);
  auto res = parser();

  std::string inputs(input);

  CAPTURE(inputs);
  CAPTURE(res.message);
  REQUIRE(!res);
}

TEST_CASE("Parse formula containing lisp code") {
  std::string_view input = GENERATE(
    "a & f(b-var \"b\")",
    "f(b-var \"a\") & b",
    "(a & f(b-var \"b\"))",
    "f(b-var \"a\") & f(b-var \"b\")",
    "f(b-and (b-var \"a\") (b-var \"b\"))",
    "f(b-and (b-var \"a\") (b-var \"b\")) :(b-and (b-var \"a\") (b-var \"b\"))",
    "f(b-and (b-var \"a\") (b-var \"b\")) :(b-and (b-var \"aa\") (b-var "
    "\"bb\")) :(b-and (b-var \"a\") (b-var \"b\"))",
    "f(b-and (b-var \"a\") (b-var \"b_\")) :(rename ** \"b_\" \"b\")");
  std::string inputs(input);
  auto is = isviewstream(input);
  boole parser(is);
  parser.eval(true);
  auto res = parser();

  CAPTURE(inputs);
  CAPTURE(res.message);
  REQUIRE(res);
  if(res) {
    std::stringstream f;
    f << *res;
    REQUIRE(f.str() == "a & b");
  }
  REQUIRE((*res)->and_inside);
}

TEST_CASE("Parse a formula with modifiers on a variable") {
  std::string_view input = "# bv{1}[1] ? bv{2}[1] bv{1}[1] & bv{2}[1]";
  auto is = isviewstream(input);
  boole parser(is);
  auto root = parser();

  CAPTURE(root.message);
  CAPTURE(*root);

  REQUIRE((*root)->type == op_type::Forall);

  auto f1 = root->right();
  auto ref = f1.right();

  REQUIRE(ref->type == op_type::And);

  auto l = ref.left();
  auto r = ref.right();

  REQUIRE(l->type == op_type::Var);
  REQUIRE(r->type == op_type::Var);
  REQUIRE(l->var.v == r->var.v);
  REQUIRE(l->var.i == 1);
  REQUIRE(l->var.q == 1);
  REQUIRE(r->var.i == 2);
  REQUIRE(r->var.q == 1);
}
