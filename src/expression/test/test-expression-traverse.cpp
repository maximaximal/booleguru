#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_all.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <catch2/internal/catch_assertion_handler.hpp>

using namespace Catch::Matchers;

using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Stackful Postorder expression tree traversal") {
  std::vector<uint32_t> log;
  std::vector<uint32_t> expected = { 1, 2, 3, 4 };

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);

  op_ref i = impl(a, b);

  op_ref n = !i;

  ops.traverse_postorder_with_stack(n.get_id(),
                                    [&log](op_manager* ops, uint32_t i) {
                                      (void)ops;
                                      log.push_back(i);
                                    });

  CAPTURE(log);

  REQUIRE_THAT(log, Equals(expected));
}
