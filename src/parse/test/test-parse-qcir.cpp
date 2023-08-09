#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/parse/qcir.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;

TEST_CASE("Parse example QCIR formulas", "[parser][qcir]") {
  std::string_view input = GENERATE("#QCIR-G14\n"
                                    "forall(a, b)\n"
                                    "exists(g1)\n"
                                    "output(g2)\n"
                                    "g1 = and(a, b)\n"
                                    "g2 = or(g1, c)",

                                    "# Just some comment..?\n"
                                    "#QCIR-14 23\n"
                                    "output(g)\n\n"
                                    "g = or()\n\n",

                                    "# Just some comment..?\n"
                                    "#  QCIR-13  \n"
                                    " # Some more comments here,\n"
                                    "# and here\n"
                                    "output( g )\n\n"
                                    "g=and()");
  auto is = isviewstream(input);
  qcir parser(is);
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
  CAPTURE(res.message);
  REQUIRE(res);

  // printf("%s\n", stringified.c_str());
}
