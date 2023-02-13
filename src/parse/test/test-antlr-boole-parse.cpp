#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/util/istringviewstream.hpp>
#include <booleguru/lua/lua-context.hpp>

#include <boole_lexer.h>
#include <boole_parser.h>

using namespace booleguru::expression;
using namespace booleguru::lua;
using namespace booleguru::parse::antlr;
using namespace antlr4;

TEST_CASE("Use the ANTLR4 boole parser directly") {
  std::shared_ptr<op_manager> ops = std::make_shared<op_manager>();
  std::shared_ptr<lua_context> lua_ctx = std::make_shared<lua_context>();

  std::string_view input_str = GENERATE("a & b",
                                        "a & !b",
                                        "a & (a | b)",
                                        "a & b & c <-> d | (e -> f)",
                                        "?a (a)",
                                        "(?a a) :F(print *last-op*)",
                                        "#d ((?a ?b (a & b)) | d)");

  isviewstream ins(input_str);
  ANTLRInputStream input(ins);
  boole_lexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  boole_parser parser(&tokens);

  // This is required!!
  parser.ops = ops;
  parser.lua = lua_ctx;

  parser.setBuildParseTree(false);
  op_ref op = parser.formula()->o;

  REQUIRE(op.to_string() == input_str);
}
