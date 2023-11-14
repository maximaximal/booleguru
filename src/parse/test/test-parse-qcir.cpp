#include <iostream>
#include <sstream>

#include <catch2/benchmark/catch_benchmark.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/cli/input_file.hpp>
#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/lua/lua-context.hpp>
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
        return ops.get(op(op_type::Var, var_manager::LITERAL_BOTTOM, 0, 0));
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
        return ops.get(op(op_type::Var, var_manager::LITERAL_TOP, 0, 0));
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
        return ops.get(op(op_type::Var, var_manager::LITERAL_BOTTOM, 0, 0));
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

constexpr static QCIRTestSnippet test_normal_form_fuzzed
  = { .str = R"(# stats: 4 8 2.0246496200561523
#QCIR-14
forall (1, 2, 3, 4)
exists (5, 6, 7, 8, 9, 10, 11, 12)
output (13)
16 = OR(2,-5,-12)
17 = OR(-4,-11,-8)
15 = AND(16, 17)
19 = OR(3,-6,9)
20 = OR(-1,-7,-10)
18 = AND(19, 20)
14 = OR(15, 18)
23 = OR(2,12,7)
24 = OR(-4,11,10)
22 = AND(23, 24)
26 = OR(1,-9,8)
27 = OR(3,-6,-5)
25 = AND(26, 27)
21 = OR(22, 25)
30 = OR(-1,-7,-5)
31 = OR(3,8,10)
29 = AND(30, 31)
33 = OR(2,-6,-12)
34 = OR(-4,-9,11)
32 = AND(33, 34)
28 = OR(29, 32)
37 = OR(2,7,-6)
38 = OR(1,-9,10)
36 = AND(37, 38)
40 = OR(4,11,-5)
41 = OR(-3,8,12)
39 = AND(40, 41)
35 = OR(36, 39)
44 = OR(1,10,5)
45 = OR(-4,8,9)
43 = AND(44, 45)
47 = OR(-3,7,12)
48 = OR(2,11,-6)
46 = AND(47, 48)
42 = OR(43, 46)
51 = OR(-3,12,-7)
52 = OR(-4,10,11)
50 = AND(51, 52)
54 = OR(-2,8,5)
55 = OR(1,-9,6)
53 = AND(54, 55)
49 = OR(50, 53)
58 = OR(-1,12,7)
59 = OR(4,-8,-10)
57 = AND(58, 59)
61 = OR(2,-11,9)
62 = OR(-3,-5,6)
60 = AND(61, 62)
56 = OR(57, 60)
65 = OR(2,11,-8)
66 = OR(-3,9,-7)
64 = AND(65, 66)
68 = OR(-4,6,12)
69 = OR(1,5,-10)
67 = AND(68, 69)
63 = OR(64, 67)
72 = OR(3,-7,12)
73 = OR(4,11,-10)
71 = AND(72, 73)
75 = OR(1,6,9)
76 = OR(-2,8,-5)
74 = AND(75, 76)
70 = OR(71, 74)
79 = OR(3,5,-6)
80 = OR(4,-10,12)
78 = AND(79, 80)
82 = OR(1,9,8)
83 = OR(-2,-11,7)
81 = AND(82, 83)
77 = OR(78, 81)
86 = OR(1,-6,-11)
87 = OR(-4,9,7)
85 = AND(86, 87)
89 = OR(2,-12,-5)
90 = OR(-3,-8,10)
88 = AND(89, 90)
84 = OR(85, 88)
93 = OR(-2,8,-5)
94 = OR(-4,11,7)
92 = AND(93, 94)
96 = OR(3,-6,-9)
97 = OR(1,-10,-12)
95 = AND(96, 97)
91 = OR(92, 95)
100 = OR(4,-8,-12)
101 = OR(-3,11,7)
99 = AND(100, 101)
103 = OR(-2,-10,9)
104 = OR(-1,-5,6)
102 = AND(103, 104)
98 = OR(99, 102)
107 = OR(3,-8,-5)
108 = OR(-1,7,11)
106 = AND(107, 108)
110 = OR(4,-10,-9)
111 = OR(-2,-6,-12)
109 = AND(110, 111)
105 = OR(106, 109)
114 = OR(-3,10,-12)
115 = OR(4,-6,-9)
113 = AND(114, 115)
117 = OR(2,-5,8)
118 = OR(1,-11,7)
116 = AND(117, 118)
112 = OR(113, 116)
121 = OR(3,-10,-7)
122 = OR(-1,-8,-5)
120 = AND(121, 122)
124 = OR(2,-12,6)
125 = OR(4,9,11)
123 = AND(124, 125)
119 = OR(120, 123)
13 = AND(14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98, 105, 112, 119))",
      .should_parse = true,
      .produce_result = *[](op_manager& ops) {
        auto const &v_1
          = "1"_var(ops),
          v_2 = "2"_var(ops), v_3 = "3"_var(ops), v_4 = "4"_var(ops),
          v_5 = "5"_var(ops), v_6 = "6"_var(ops), v_7 = "7"_var(ops),
          v_8 = "8"_var(ops), v_9 = "9"_var(ops), v_10 = "10"_var(ops),
          v_11 = "11"_var(ops), v_12 = "12"_var(ops),
          g_16 = v_2 || !v_5 || !v_12, g_17 = !v_4 || !v_11 || !v_8,
          g_15 = g_16 && g_17, g_19 = v_3 || !v_6 || v_9,
          g_20 = !v_1 || !v_7 || !v_10, g_18 = g_19 && g_20,
          g_14 = g_15 || g_18, g_23 = v_2 || v_12 || v_7,
          g_24 = !v_4 || v_11 || v_10, g_22 = g_23 && g_24,
          g_26 = v_1 || !v_9 || v_8, g_27 = v_3 || !v_6 || !v_5,
          g_25 = g_26 && g_27, g_21 = g_22 || g_25, g_30 = !v_1 || !v_7 || !v_5,
          g_31 = v_3 || v_8 || v_10, g_29 = g_30 && g_31,
          g_33 = v_2 || !v_6 || !v_12, g_34 = !v_4 || !v_9 || v_11,
          g_32 = g_33 && g_34, g_28 = g_29 || g_32, g_37 = v_2 || v_7 || !v_6,
          g_38 = v_1 || !v_9 || v_10, g_36 = g_37 && g_38,
          g_40 = v_4 || v_11 || !v_5, g_41 = !v_3 || v_8 || v_12,
          g_39 = g_40 && g_41, g_35 = g_36 || g_39, g_44 = v_1 || v_10 || v_5,
          g_45 = !v_4 || v_8 || v_9, g_43 = g_44 && g_45,
          g_47 = !v_3 || v_7 || v_12, g_48 = v_2 || v_11 || !v_6,
          g_46 = g_47 && g_48, g_42 = g_43 || g_46, g_51 = !v_3 || v_12 || !v_7,
          g_52 = !v_4 || v_10 || v_11, g_50 = g_51 && g_52,
          g_54 = !v_2 || v_8 || v_5, g_55 = v_1 || !v_9 || v_6,
          g_53 = g_54 && g_55, g_49 = g_50 || g_53, g_58 = !v_1 || v_12 || v_7,
          g_59 = v_4 || !v_8 || !v_10, g_57 = g_58 && g_59,
          g_61 = v_2 || !v_11 || v_9, g_62 = !v_3 || !v_5 || v_6,
          g_60 = g_61 && g_62, g_56 = g_57 || g_60, g_65 = v_2 || v_11 || !v_8,
          g_66 = !v_3 || v_9 || !v_7, g_64 = g_65 && g_66,
          g_68 = !v_4 || v_6 || v_12, g_69 = v_1 || v_5 || !v_10,
          g_67 = g_68 && g_69, g_63 = g_64 || g_67, g_72 = v_3 || !v_7 || v_12,
          g_73 = v_4 || v_11 || !v_10, g_71 = g_72 && g_73,
          g_75 = v_1 || v_6 || v_9, g_76 = !v_2 || v_8 || !v_5,
          g_74 = g_75 && g_76, g_70 = g_71 || g_74, g_79 = v_3 || v_5 || !v_6,
          g_80 = v_4 || !v_10 || v_12, g_78 = g_79 && g_80,
          g_82 = v_1 || v_9 || v_8, g_83 = !v_2 || !v_11 || v_7,
          g_81 = g_82 && g_83, g_77 = g_78 || g_81, g_86 = v_1 || !v_6 || !v_11,
          g_87 = !v_4 || v_9 || v_7, g_85 = g_86 && g_87,
          g_89 = v_2 || !v_12 || !v_5, g_90 = !v_3 || !v_8 || v_10,
          g_88 = g_89 && g_90, g_84 = g_85 || g_88, g_93 = !v_2 || v_8 || !v_5,
          g_94 = !v_4 || v_11 || v_7, g_92 = g_93 && g_94,
          g_96 = v_3 || !v_6 || !v_9, g_97 = v_1 || !v_10 || !v_12,
          g_95 = g_96 && g_97, g_91 = g_92 || g_95,
          g_100 = v_4 || !v_8 || !v_12, g_101 = !v_3 || v_11 || v_7,
          g_99 = g_100 && g_101, g_103 = !v_2 || !v_10 || v_9,
          g_104 = !v_1 || !v_5 || v_6, g_102 = g_103 && g_104,
          g_98 = g_99 || g_102, g_107 = v_3 || !v_8 || !v_5,
          g_108 = !v_1 || v_7 || v_11, g_106 = g_107 && g_108,
          g_110 = v_4 || !v_10 || !v_9, g_111 = !v_2 || !v_6 || !v_12,
          g_109 = g_110 && g_111, g_105 = g_106 || g_109,
          g_114 = !v_3 || v_10 || !v_12, g_115 = v_4 || !v_6 || !v_9,
          g_113 = g_114 && g_115, g_117 = v_2 || !v_5 || v_8,
          g_118 = v_1 || !v_11 || v_7, g_116 = g_117 && g_118,
          g_112 = g_113 || g_116, g_121 = v_3 || !v_10 || !v_7,
          g_122 = !v_1 || !v_8 || !v_5, g_120 = g_121 && g_122,
          g_124 = v_2 || !v_12 || v_6, g_125 = v_4 || v_9 || v_11,
          g_123 = g_124 && g_125, g_119 = g_120 || g_123,
          g_13 = g_14 && g_21 && g_28 && g_35 && g_42 && g_49 && g_56 && g_63
                 && g_70 && g_77 && g_84 && g_91 && g_98 && g_105 && g_112
                 && g_119;
        return forall(
          v_1,
          forall(
            v_2,
            forall(
              v_3,
              forall(
                v_4,
                exists(
                  v_5,
                  exists(
                    v_6,
                    exists(
                      v_7,
                      exists(v_8,
                             exists(v_9,
                                    exists(v_10,
                                           exists(v_11,
                                                  exists(v_12, g_13))))))))))));
      } };

constexpr static std::initializer_list<QCIRTestSnippet> test_snippets
  = { test1,
      test2,
      test3,
      test6,
      test7,
      test12,
      test13,
      test14,
      test15,
      test18,
      test19,
      test23,
      test29,
      test35,
      test36,
      test45,
      test46,
      test47,
      test48,
      test53,
      test54,
      test55,
      test56,
      test58,
      test59,
      test_non_cnf_qbf,
      test_normal_form_fuzzed };

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

inline op_ref
read_compressed_file(std::string_view const& path,
                     std::shared_ptr<op_manager> const& ops) {
  std::shared_ptr<booleguru::lua::lua_context> lua
    = std::make_shared<booleguru::lua::lua_context>(ops);
  booleguru::cli::input_file infile(path, ops, lua);
  return infile.process();
};

TEST_CASE("Parse big QCIR formulas", "[parser][qcir][!benchmark]") {
  BENCHMARK("Big") {
    op_ref op = read_compressed_file("./src/parse/test/artifacts/0.1.qcir.gz",
                                     std::make_shared<op_manager>());
    REQUIRE(op.valid());
    return op;
  };
  BENCHMARK("Huge") {
    op_ref op = read_compressed_file(
      "./src/parse/test/artifacts/voter_size_202219_1U43.qcir.gz",
      std::make_shared<op_manager>());
    REQUIRE(op.valid());
    return op;
  };
  BENCHMARK("Gigantic") {
    op_ref op = read_compressed_file("./src/parse/test/artifacts/11.1.qcir.gz",
                                     std::make_shared<op_manager>());
    REQUIRE(op.valid());
    return op;
  };
}
