#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/serialize/qdimacs.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/var_manager.hpp>

using namespace booleguru::serialize;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Serialize simple example QDIMACS formula", "[serialize][qdimacs]") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  op_ref const &x = "x"_var(ops), &y = "y"_var(ops), &z = "z"_var(ops);
  op_ref const &op0 = x && y || z, &op1 = op0 || !(z && op0),
               &op2 = ops->get(op(op_type::Exists, x.get_id(), op1.get_id())),
               &op3 = ops->get(op(op_type::Forall, y.get_id(), op2.get_id())),
               &op4 = ops->get(op(op_type::Forall, z.get_id(), op3.get_id()));

  CAPTURE(op4);

  std::stringstream o;
  booleguru::serialize::qdimacs serializer(o);
  serializer(op4);

  CAPTURE(o.str());

  const char* expected = R"(c 1 x
c 2 y
c 3 z
p cnf 8 15
a 3 2 0
e 1 4 5 6 7 8 0
-1 -2 4 0
1 -4 0
2 -4 0
4 3 -5 0
-4 5 0
-3 5 0
-3 -5 6 0
3 -6 0
5 -6 0
-6 -7 0
6 7 0
5 7 -8 0
-5 8 0
-7 8 0
8 0
)";

  REQUIRE(o.str() == expected);
}
