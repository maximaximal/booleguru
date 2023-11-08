#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/serialize/qcir.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/var_manager.hpp>

using namespace booleguru::serialize;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Serialize simple example QCIR formula", "[serialize][qcir]") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_x1 = ops->vars().get(variable{ "x1" });
  auto var_x2 = ops->vars().get(variable{ "x2" });
  auto var_x3 = ops->vars().get(variable{ "x3" });
  auto var_z = ops->vars().get(variable{ "z" });

  auto op_x1 = ops->get(op(op_type::Var, var_x1.get_id(), 0, 0));
  auto op_x2 = ops->get(op(op_type::Var, var_x2.get_id(), 0, 0));
  auto op_x3 = ops->get(op(op_type::Var, var_x3.get_id(), 0, 0));
  auto op_z = ops->get(op(op_type::Var, var_z.get_id(), 0, 0));

  auto op_g1_ = ops->get(op(op_type::And, op_x1.get_id(), op_x2.get_id()));
  auto op_g1 = ops->get(op(op_type::And, op_g1_.get_id(), op_x3.get_id()));

  auto op_g2_ = ops->get(op(op_type::Exists, op_x2.get_id(), op_g1.get_id()));
  auto op_g2 = ops->get(op(op_type::Exists, op_x1.get_id(), op_g2_.get_id()));

  auto op_g3 = op_z && op_g2;

  auto op_complete
    = ops->get(op(op_type::Forall, op_z.get_id(), op_g3.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(op_complete);

  CAPTURE(op_complete);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 7
# 1 z
# 2 x1
# 3 x2
# 4 x3
free(4)
forall(1)
output(7)
5 = and(2, 3, 4)
6 = exists(2, 3; 5)
7 = and(1, 6)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple example QCIR formula with some NOTs",
          "[serialize][qcir]") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_x1 = ops->vars().get(variable{ "x1" });
  auto var_x2 = ops->vars().get(variable{ "x2" });
  auto var_x3 = ops->vars().get(variable{ "x3" });
  auto var_z = ops->vars().get(variable{ "z" });

  auto op_x1 = ops->get(op(op_type::Var, var_x1.get_id(), 0, 0));
  auto op_x2 = ops->get(op(op_type::Var, var_x2.get_id(), 0, 0));
  auto op_x3 = ops->get(op(op_type::Var, var_x3.get_id(), 0, 0));
  auto op_z = ops->get(op(op_type::Var, var_z.get_id(), 0, 0));

  auto op_g1_ = ops->get(op(op_type::And, op_x1.get_id(), op_x2.get_id()));
  auto op_g1 = ops->get(op(op_type::And, op_g1_.get_id(), op_x3.get_id()));

  auto op_g2_ = ops->get(op(op_type::Exists, op_x2.get_id(), op_g1.get_id()));
  auto op_g2 = ops->get(op(op_type::Exists, op_x1.get_id(), op_g2_.get_id()));

  auto op_g3 = !(op_z && !op_g2);

  auto op_complete
    = ops->get(op(op_type::Forall, op_z.get_id(), op_g3.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(op_complete);

  CAPTURE(op_complete);
  // REQUIRE(false);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 7
# 1 z
# 2 x1
# 3 x2
# 4 x3
free(4)
forall(1)
output(-7)
5 = and(2, 3, 4)
6 = exists(2, 3; 5)
7 = and(1, -6)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple prenex example QCIR formula", "[serialize][qcir]") {
  std::shared_ptr<op_manager> ops
    = std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_v1 = ops->vars().get(variable{ "v1" });
  auto var_v2 = ops->vars().get(variable{ "v2" });
  auto var_v3 = ops->vars().get(variable{ "v3" });

  auto op_v1 = ops->get(op(op_type::Var, var_v1.get_id(), 0, 0));
  auto op_v2 = ops->get(op(op_type::Var, var_v2.get_id(), 0, 0));
  auto op_v3 = ops->get(op(op_type::Var, var_v3.get_id(), 0, 0));

  auto op_g1 = op_v1 && op_v2;
  auto op_g2 = !op_v1 && !op_v2 && op_v3;
  auto op_ = op_g1 || op_g2;

  auto ex_v3 = ops->get(op(op_type::Exists, op_v3.get_id(), op_.get_id()));
  auto ex_v2 = ops->get(op(op_type::Exists, op_v2.get_id(), ex_v3.get_id()));
  auto forall_v1
    = ops->get(op(op_type::Forall, op_v1.get_id(), ex_v2.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(forall_v1);

  CAPTURE(forall_v1);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 6
# 1 v1
# 2 v2
# 3 v3
forall(1)
exists(2, 3)
output(6)
4 = and(1, 2)
5 = and(-1, -2, 3)
6 = or(4, 5)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple non prenex CNF example to QCIR formula cleansed",
          "[serialize][qcir]") {
  op_manager ops;

  op_ref x = "x"_var(ops);
  op_ref y = "y"_var(ops);
  op_ref z = "z"_var(ops);

  auto op_complete = forall(z, z || exists(x, exists(y, x && y && z)));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(op_complete);

  CAPTURE(op_complete);
  // REQUIRE(false);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 6
# 1 z
# 2 x
# 3 y
forall(1)
output(6)
4 = and(2, 3, 1)
5 = exists(2, 3; 4)
6 = or(1, 5)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Non prenex non CNF to QCIR", "[serialize][qcir]") {
  op_manager ops;

  op_ref x = "x"_var(ops);
  op_ref z = "z"_var(ops);

  auto formula_1 = exists(x, x ^ z);
  auto formula_2 = z ^ exists(x, x ^ z);

  auto formula = forall(z, formula_1 && formula_2);

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(formula);

  CAPTURE(formula);
  // REQUIRE(false);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 6
# 1 z
# 2 x
forall(1)
output(6)
3 = xor(2, 1)
4 = exists(2; 3)
5 = xor(1, 4)
6 = and(4, 5)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Formula with equivalence to QCIR", "[serialize][qcir]") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);
  op_ref x = "x"_var(ops);

  auto formula = (x || !a || !b || !c) && (!x || a) && (!x || b) && (!x || c);

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(formula);

  CAPTURE(formula);
  // REQUIRE(false);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 9
# 1 x
# 2 a
# 3 b
# 4 c
free(1, 2, 3, 4)
output(9)
5 = or(1, -2, -3, -4)
6 = or(-1, 2)
7 = or(-1, 3)
8 = or(-1, 4)
9 = and(5, 6, 7, 8)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Formula with similar or sub-trees converted to QCIR",
          "[serialize][qcir]") {
  op_manager ops;

  op_ref a = "a"_var(ops);
  op_ref b = "b"_var(ops);
  op_ref c = "c"_var(ops);

  op_ref l = impl(a, c);
  op_ref r = impl(b, c);

  op_ref f = equi(l, r);

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(f);

  CAPTURE(f);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 8
# 1 a
# 2 c
# 3 b
free(1, 2, 3)
output(8)
4 = or(-3, 2)
5 = or(-1, 2, -4)
6 = or(-1, 2)
7 = or(-3, 2, -6)
8 = and(5, 7)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Formula with boolean constant converted to QCIR",
          "[serialize][qcir]") {
  op_manager ops;
  auto top = ops.top();

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(top);

  CAPTURE(top);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 1
output(1)
1 = and()
)";

  REQUIRE(o.str() == expected);
}
