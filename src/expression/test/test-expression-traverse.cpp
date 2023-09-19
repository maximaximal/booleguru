#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_all.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <catch2/internal/catch_assertion_handler.hpp>

using namespace Catch::Matchers;

using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Stackful Postorder expression tree traversal") {
  std::vector<op_id> log;
  std::vector<op_id> expected = { 1, 2, 3, 4 };

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);

  op_ref i = impl(a, b);

  op_ref n = !i;

  ops.traverse_postorder_with_stack(n.get_id(),
                                    [&log](op_manager* ops, op_id i) {
                                      (void)ops;
                                      log.push_back(i);
                                    });

  CAPTURE(log);

  REQUIRE_THAT(log, Equals(expected));
}

TEST_CASE("Stackful Postorder expression tree traversal with modification 1") {
  std::vector<op_id> log;

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);

  op_ref i = impl(a, b);

  op_ref n = !i;

  op_id root = ops.traverse_postorder_with_stack(
    n.get_id(), [&log, &b, &c](op_manager* ops, op_id i) -> op_id {
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
  std::vector<op_id> log;

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);

  op_ref i = lpmi(a, b);

  op_ref n = !i;

  op_id root = ops.traverse_postorder_with_stack(
    n.get_id(), [&log, &b](op_manager* ops, op_id i) -> op_id {
      op_ref r = (*ops)[i];
      if(r->type == op_type::Lpmi) {
        return ops->get_id(op(op_type::Impl, r->left(), r->right()));
      }
      return i;
    });

  CAPTURE(log);

  REQUIRE(ops[root].to_string() == "!(a -> b)");
}

TEST_CASE("Stackful Postorder expression tree traversal with modification 3") {
  std::vector<op_id> log;

  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);

  op_ref t0 = ((a && b || c) && (b && c || a)) && c;
  op_ref t1 = t0 && !t0;
  op_ref t2 = lpmi(forall(a, t1), exists(a, t1));

  op_id root = ops.traverse_postorder_with_stack(
    t2.get_id(), [&log, &b](op_manager* ops, op_id i) -> op_id {
      op_ref r = (*ops)[i];
      if(r->type == op_type::Lpmi) {
        // Flip the lpmi to impl
        return ops->get_id(op(op_type::Impl, r->left(), r->right()));
      } else if(r->type == op_type::Not) {
        // Remove the not
        return r->left();
      }
      return i;
    });

  std::string s{ ops[root].to_string() };
  REQUIRE(s.find("<-") == std::string::npos);
}
