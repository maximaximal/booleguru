#include <iostream>
#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/parse/qcir.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru::parse;
using namespace booleguru::expression::literals;
using booleguru::expression::op;
using booleguru::expression::op_manager;
using booleguru::expression::op_ref;
using booleguru::expression::op_type;
using booleguru::expression::var_manager;

using QCIRTestSnippet = struct QCIRTestSnippet__ {
  std::string_view const str;
  bool const should_parse;
  op_ref (*produce_result)(op_manager& ops);
};

constexpr static QCIRTestSnippet test1
  = { .str = R"(#QCIR-G14
forall(a, b)
exists(c)
output(g2)
g1 = and(a, b)
g2 = or(g1, c)
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const a = "a"_var(ops), b = "b"_var(ops), c = "c"_var(ops),
                   g1 = a && b, g2 = g1 || c;
        return forall(a, forall(b, exists(c, g2)));
      } };

constexpr static QCIRTestSnippet test2
  = { .str = R"(# Just some comment..?
#QCIR-14 23
output(g)
g = or()
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        return ops.get(op(op_type::Var, var_manager::LITERAL_BOTTOM, 0));
      } };

constexpr static QCIRTestSnippet test3
  = { .str = R"(# Just some comment..?
#  QCIR-13
 # More comments
# More
output( 23 )
23=and()
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        return ops.get(op(op_type::Var, var_manager::LITERAL_TOP, 0));
      } };

constexpr static QCIRTestSnippet test6 = { .str = R"(#QCIR-14
output()
)",
                                           .should_parse = false,
                                           .produce_result = nullptr };

constexpr static QCIRTestSnippet test7 = { .str = R"(#QCIR-14
output(a)
)",
                                           .should_parse = false,
                                           .produce_result = nullptr };

constexpr static QCIRTestSnippet test12
  = { .str = R"(#QCIR-G14
forall(q)
exists(r)
forall(s)
output(a)
a = or(q)
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const q = "q"_var(ops), r = "r"_var(ops), s = "s"_var(ops);
        return forall(q, exists(r, forall(s, q)));
      } };

constexpr static QCIRTestSnippet test13
  = { .str = R"(#QCIR-G14
free(q)
exists(r, s)
output(a)
b = and(r, s)
a = or(q, b)
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const q = "q"_var(ops), r = "r"_var(ops), s = "s"_var(ops),
                   b = r && s, a = q || b;
        return exists(r, exists(s, a));
      } };

constexpr static QCIRTestSnippet test14
  = { .str = R"(#QCIR-G14
exists(q)
exists(r)
output(a)
a = or(q, r)
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const q = "q"_var(ops), r = "r"_var(ops), a = q || r;
        return exists(q, exists(r, a));
      } };

constexpr static QCIRTestSnippet test15 = { .str = R"(#QCIR-G14
exists(q)
free(r)
output(a)
a = or(q)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test18 = { .str = R"(#QCIR-G14
output(g)
g = or(a, b, c, d)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test19 = { .str = R"(#QCIR-G14
exists()
output(g)
g = or()
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test23 = { .str = R"(#QCIR-G14
exists(a,
output(g)
g = or()
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test29
  = { .str = R"(#QCIR-G14
output(a)
a = or()
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        return ops.get(op(op_type::Var, var_manager::LITERAL_BOTTOM, 0));
      } };

constexpr static QCIRTestSnippet test34 = { .str = R"(#QCIR-G14
output(a)
a
=   and()
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test35 = { .str = R"(#QCIR-G14
output(-a)
a = and(b, c, -d,e)
o = or(-a)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test36 = { .str = R"(
x=xor(-a,-o)
i   = ite(d, f, -f)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test45 = { .str = R"(#QCIR-G14
output(a)
a = or(b)
a=and(b)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test46 = { .str = R"(#QCIR-G14
output(a)
a = or(b)
a=and(b)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test47 = { .str = R"(#QCIR-G14
output(a)
a = or(b ,        c)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test48 = { .str = R"(#QCIR-G14
output(a)
a = or(b,c,c,c)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test53 = { .str = R"(#QCIR-G14
output(a)
a = forall(b; c, d)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test54 = { .str = R"(#QCIR-G14
output(a)
a = exists(a; b)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test55 = { .str = R"(#QCIR-G14
free(b)
output(a)
a = exists(b; c)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test56 = { .str = R"(#QCIR-G14
exists(b)
output(a)
a = exists(b; c)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test58
  = { .str = R"(#QCIR-G14
output(a)
d = and(b, c, f)
e = forall(f; -d)
a = exists(b, c; -e)
)",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const b = "b"_var(ops), c = "c"_var(ops), f = "f"_var(ops),
                   d = b && c && f, e = forall(f, !d),
                   a = exists(b, exists(c, !e));
        return a;
      } };

constexpr static QCIRTestSnippet test59 = { .str = R"(#QCIR-G14
exists(4)
output(5)
5 = or(4, 5)
)",
                                            .should_parse = false,
                                            .produce_result = nullptr };

constexpr static QCIRTestSnippet test_non_cnf_qbf
  = { .str = R"(#QCIR-14
output(-g5)
g1 = xor(x, z)
g2 = exists(x; g1)
g3 = xor(z, g2)
g4 = and(g2, g3)
g5 = forall(z; g4))",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const x = "x"_var(ops), z = "z"_var(ops), g1 = x ^ z,
                   g2 = exists(x, g1), g3 = z ^ g2, g4 = g2 && g3,
                   g5 = forall(z, g4);
        return !g5;
      } };

constexpr static std::initializer_list<QCIRTestSnippet> test_snippets
  = { test1,  test2,  test3,  test6,  test7,           test12, test13,
      test14, test15, test18, test19, test23,          test29, test35,
      test36, test45, test46, test47, test48,          test53, test54,
      test55, test56, test58, test59, test_non_cnf_qbf };

TEST_CASE("Parse example QCIR formulas", "[parser][qcir]") {
  uint32_t i = 0;
  for(const QCIRTestSnippet input : test_snippets) {
    DYNAMIC_SECTION("Test formula " << i++) {
      std::shared_ptr<op_manager> ops = std::make_shared<op_manager>();
      auto is = isviewstream(input.str);
      qcir parser(is, ops);
      auto res = parser();

      CAPTURE(input.str);
      CAPTURE(input.should_parse);
      if(res) {
        CAPTURE(*res);
        CAPTURE(res->to_string());
      }
      CAPTURE(res.message);
      if(input.should_parse) {
        REQUIRE(res);
        if(input.produce_result != nullptr) {
          REQUIRE(*res == input.produce_result(*ops));
        }
      } else {
        REQUIRE_FALSE(res);
      }
    }
  }
}

