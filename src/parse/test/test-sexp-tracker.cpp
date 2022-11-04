#include <catch2/catch_test_macros.hpp>

#include <booleguru/parse/sexpr-tracker.hpp>

using namespace booleguru::parse;

TEST_CASE("Parse string with SEXP using only sexp-tracker") {
  std::string_view s = "a(a b +)";
  sexpr_tracker t;
  for(size_t i = 0; i < s.size(); ++i) {
    t.append(s[i]);
  }

  REQUIRE(std::string(t.str()) == "(a b +)");
}
