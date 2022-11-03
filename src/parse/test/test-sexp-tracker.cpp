#include <catch2/catch_test_macros.hpp>

#include <booleguru/parse/sexpr-tracker.hpp>

using namespace booleguru::parse;

TEST_CASE("Parse string with SEXP using only sexp-tracker") {
  std::string_view s = "a(a b +)";
  sexpr_tracker t;
  for(size_t i = 0; i < 4; ++i) {
    CAPTURE(i);
    CAPTURE(s[i]);
    REQUIRE(!t.append(s[i]));
  }

  REQUIRE(t.stop());
  REQUIRE(t.stopped());

  for(size_t i = 4; i < 7; ++i) {
    CAPTURE(i);
    CAPTURE(s[i]);
    REQUIRE(t.append(s[i]));
  }

  REQUIRE(!t.append(s[7]));
  REQUIRE(t.stopped());

  REQUIRE(std::string(t.str()) == "(a b +)");

  t.stop_handled();

  REQUIRE(!t.stopped());
}

TEST_CASE("Parse string with SEXP using only sexp-tracker starting from (") {
  std::string_view s = "a(a b +)";
  sexpr_tracker t;
  for(size_t i = 0; i < 2; ++i) {
    CAPTURE(i);
    CAPTURE(s[i]);
    REQUIRE(!t.append(s[i]));
  }

  REQUIRE(t.stop());
  REQUIRE(t.stopped());

  for(size_t i = 2; i < 7; ++i) {
    CAPTURE(i);
    CAPTURE(s[i]);
    REQUIRE(t.append(s[i]));
  }

  REQUIRE(!t.append(s[7]));
  REQUIRE(t.stopped());

  REQUIRE(std::string(t.str()) == "(a b +)");

  t.stop_handled();

  REQUIRE(!t.stopped());
}
