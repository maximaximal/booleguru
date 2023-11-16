#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/smtlib2.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

static const std::string_view test_simple_boolean = R"((set-logic BV)
(declare-const a Bool)
(declare-const b Bool)
(assert (and a b))
)";

static const std::string_view test_simple_bitvec = R"((set-logic BV)
(declare-const a (_ BitVec 2))
(declare-const b (_ BitVec 2))
(assert (bvand a b))
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

  REQUIRE(res);
  CAPTURE(res->to_string());

  REQUIRE(false);
}
