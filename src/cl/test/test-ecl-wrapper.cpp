#include <catch2/catch_test_macros.hpp>

#include <booleguru/cl/ecl-wrapper.hpp>

using namespace booleguru::cl;

TEST_CASE("Run ECL test function") {
  ecl_wrapper &w = ecl_wrapper::get();
  auto result = w.eval("(test-func-arg-incr 1)");
  int result_int = std::get<long>(result);
  REQUIRE(result_int == 2);
}
