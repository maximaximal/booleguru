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

TEST_CASE("Stackful Postorder expression tree traversal with modification 1") {
  std::vector<uint32_t> log;

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);

  op_ref i = impl(a, b);

  op_ref n = !i;

  uint32_t root = ops.traverse_postorder_with_stack(
    n.get_id(), [&log, &b, &c](op_manager* ops, uint32_t i) -> uint32_t {
      (void)ops;

      if(i == b.get_id()) {
        return c.get_id();
      }
      return i;
    });

  CAPTURE(log);

  REQUIRE(ops[root].to_string() == "!(a -> c)");
}

TEST_CASE("Stackful Postorder expression tree traversal with modification 2") {
  std::vector<uint32_t> log;

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);

  op_ref i = lpmi(a, b);

  op_ref n = !i;

  uint32_t root = ops.traverse_postorder_with_stack(
    n.get_id(), [&log, &b](op_manager* ops, uint32_t i) -> uint32_t {
      op_ref r = (*ops)[i];
      if(r->type == op_type::Lpmi) {
        return ops->get_id(op(op_type::Impl, r->left(), r->right()));
      }
      return i;
    });

  CAPTURE(log);

  REQUIRE(ops[root].to_string() == "!(a -> b)");
}
