#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/aiger.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

static const std::string_view test_and = R"(aag 3 2 0 1 1
2
4
6
6 2 4
)";

static const std::string_view test_and_symb = R"(aag 3 2 0 1 1
2
4
6
6 2 4
i0 x
i1 y
o0 o
)";

static const std::string_view test_or = R"(aag 3 2 0 1 1
2
4
7
6 3 5
)";

static const std::string_view test_true = R"(aag 0 0 0 1 0
1
)";

static const std::string_view test_false = R"(aag 0 0 0 1 0
0
)";

static const std::string_view test_inverter = R"(aag 1 1 0 1 0
2
3
)";

TEST_CASE("Parse simple aiger files with AND gates", "[aiger]") {
  auto is = isviewstream(GENERATE(test_and, test_and_symb));
  aiger parser(is);
  auto res = parser();

  REQUIRE(res);
  CAPTURE(res->to_string());
  REQUIRE((*res)->type == op_type::And);
  REQUIRE(res->left()->type == op_type::Var);
  REQUIRE(res->right()->type == op_type::Var);
}

TEST_CASE("Parse simple aiger files with OR gates", "[aiger]") {
  auto is = isviewstream(test_or);
  aiger parser(is);
  auto res = parser();

  REQUIRE(res);
  CAPTURE(res->to_string());
  REQUIRE((*res)->type == op_type::Not);
  REQUIRE((*res).left()->type == op_type::And);
  REQUIRE(res->left().left()->type == op_type::Not);
  REQUIRE(res->left().right()->type == op_type::Not);
  REQUIRE(res->left().left().left()->type == op_type::Var);
  REQUIRE(res->left().right().left()->type == op_type::Var);
}

TEST_CASE("Parse constant true aiger", "[aiger]") {
  auto is = isviewstream(test_true);
  aiger parser(is);
  auto res = parser();

  REQUIRE(res);
  CAPTURE(res->to_string());
  REQUIRE((*res)->type == op_type::Var);
  REQUIRE((*res)->var.v == 1);
}

TEST_CASE("Parse constant false aiger", "[aiger]") {
  auto is = isviewstream(test_false);
  aiger parser(is);
  auto res = parser();

  REQUIRE(res);
  CAPTURE(res->to_string());
  REQUIRE((*res)->type == op_type::Var);
  REQUIRE((*res)->var.v == 2);
}

TEST_CASE("Parse inverter aiger", "[aiger]") {
  auto is = isviewstream(test_inverter);
  aiger parser(is);
  auto res = parser();

  CAPTURE(res.message);
  REQUIRE(res);
  CAPTURE(res->to_string());
  REQUIRE((*res)->type == op_type::Not);
  REQUIRE(res->left()->type == op_type::Var);
}
