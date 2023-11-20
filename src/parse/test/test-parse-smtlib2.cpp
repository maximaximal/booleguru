#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

static const std::string_view test_simple_boolean = R"((set-logic BV)
(declare-const a Bool)
(declare-const b Bool)
(assert (and a b))
(check-sat)
)";

static const std::string_view test_simple_bitvec = R"((set-logic BV)
(declare-const a (_ BitVec 2))
(declare-const b (_ BitVec 2))
(assert (= (bvor a b) (_ bv2 2)))
(check-sat)
)";

static const std::string_view test_simple_bitvec_quantified = R"((set-logic BV)
(declare-const a (_ BitVec 2))
(assert (exists ((b (_ BitVec 2))) (= (bvor a b) (_ bv2 2))))
(check-sat)
)";

static const std::string_view test_duplicate_bitvec_quantified = R"((set-logic BV)
(declare-const a (_ BitVec 2))
(assert (and (exists ((b (_ BitVec 2))) (= (bvor a b) (_ bv2 2))) (exists ((b (_ BitVec 2))) (= (bvor a b) (_ bv2 2)))))
(check-sat)
)";

TEST_CASE("Parse simple SMTLIB2 file without BV", "[smtlib2]") {
  auto is = isviewstream(test_simple_boolean);
  smtlib2 parser(is);
  auto res = parser();

  REQUIRE(res);
  CAPTURE(res->to_string());
}

TEST_CASE("Parse simple SMTLIB2 file with BV", "[smtlib2]") {
  auto is = isviewstream(test_simple_bitvec);
  smtlib2 parser(is);
  auto res = parser();

  if(!res) {
    CAPTURE(res.message);
    REQUIRE(false);
  }

  REQUIRE(res);
  CAPTURE(res->to_string());
}

TEST_CASE("Parse simple SMTLIB2 file with quantified BV", "[smtlib2]") {
  auto is = isviewstream(test_simple_bitvec_quantified);
  smtlib2 parser(is);
  auto res = parser();

  if(!res) {
    CAPTURE(res.message);
    REQUIRE(false);
  }

  REQUIRE(res);
  CAPTURE(res->to_string());
}

TEST_CASE("Parse duplicate SMTLIB2 file with quantified BV", "[smtlib2]") {
  auto is = isviewstream(test_duplicate_bitvec_quantified);
  smtlib2 parser(is);
  auto res = parser();

  if(!res) {
    CAPTURE(res.message);
    REQUIRE(false);
  }

  REQUIRE(res);
  CAPTURE(res->to_string());
}
