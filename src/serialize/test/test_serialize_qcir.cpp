#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/serialize/qcir.hpp>

#include <booleguru/expression/var_manager.hpp>
#include <booleguru/expression/literals.hpp>

using namespace booleguru::serialize;
using namespace booleguru::expression;
using namespace booleguru::expression::literals;

TEST_CASE("Serialize simple example QCIR formula") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_x1 = ops->vars().get(variable{ "x1" });
  auto var_x2 = ops->vars().get(variable{ "x2" });
  auto var_x3 = ops->vars().get(variable{ "x3" });
  auto var_z = ops->vars().get(variable{ "z" });

  auto op_x1 = ops->get(op(op_type::Var, var_x1.get_id(), 0));
  auto op_x2 = ops->get(op(op_type::Var, var_x2.get_id(), 0));
  auto op_x3 = ops->get(op(op_type::Var, var_x3.get_id(), 0));
  auto op_z = ops->get(op(op_type::Var, var_z.get_id(), 0));

  auto op_g1_ = ops->get(op(op_type::And, op_x1.get_id(), op_x2.get_id()));
  auto op_g1 = ops->get(op(op_type::And, op_g1_.get_id(), op_x3.get_id()));

  auto op_g2_ = ops->get(op(op_type::Exists, var_x2.get_id(), op_g1.get_id()));
  auto op_g2 = ops->get(op(op_type::Exists, var_x1.get_id(), op_g2_.get_id()));

  auto op_g3 = op_z && op_g2;

  auto op_complete =
    ops->get(op(op_type::Forall, var_z.get_id(), op_g3.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(op_complete);

  CAPTURE(op_complete);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 7
free(3)
forall(4)
output(9)
6 = and(1, 2, 3)
8 = exists(1, 2; 6)
9 = and(4, 8)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple example QCIR formula with some NOTs") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_x1 = ops->vars().get(variable{ "x1" });
  auto var_x2 = ops->vars().get(variable{ "x2" });
  auto var_x3 = ops->vars().get(variable{ "x3" });
  auto var_z = ops->vars().get(variable{ "z" });

  auto op_x1 = ops->get(op(op_type::Var, var_x1.get_id(), 0));
  auto op_x2 = ops->get(op(op_type::Var, var_x2.get_id(), 0));
  auto op_x3 = ops->get(op(op_type::Var, var_x3.get_id(), 0));
  auto op_z = ops->get(op(op_type::Var, var_z.get_id(), 0));

  auto op_g1_ = ops->get(op(op_type::And, op_x1.get_id(), op_x2.get_id()));
  auto op_g1 = ops->get(op(op_type::And, op_g1_.get_id(), op_x3.get_id()));

  auto op_g2_ = ops->get(op(op_type::Exists, var_x2.get_id(), op_g1.get_id()));
  auto op_g2 = ops->get(op(op_type::Exists, var_x1.get_id(), op_g2_.get_id()));

  auto op_g3 = !(op_z && !op_g2);

  auto op_complete =
    ops->get(op(op_type::Forall, var_z.get_id(), op_g3.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(op_complete);

  CAPTURE(op_complete);
  // REQUIRE(false);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 7
free(3)
forall(4)
output(-10)
6 = and(1, 2, 3)
8 = exists(1, 2; 6)
10 = and(4, -8)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple prenex example QCIR formula") {
  std::shared_ptr<op_manager> ops =
    std::make_shared<op_manager>(std::make_shared<var_manager>());

  auto var_v1 = ops->vars().get(variable{ "v1" });
  auto var_v2 = ops->vars().get(variable{ "v2" });
  auto var_v3 = ops->vars().get(variable{ "v3" });

  auto op_v1 = ops->get(op(op_type::Var, var_v1.get_id(), 0));
  auto op_v2 = ops->get(op(op_type::Var, var_v2.get_id(), 0));
  auto op_v3 = ops->get(op(op_type::Var, var_v3.get_id(), 0));

  auto op_g1 = op_v1 && op_v2;
  auto op_g2 = !op_v1 && !op_v2 && op_v3;
  auto op_ = op_g1 || op_g2;

  auto ex_v3 = ops->get(op(op_type::Exists, var_v3.get_id(), op_.get_id()));
  auto ex_v2 = ops->get(op(op_type::Exists, var_v2.get_id(), ex_v3.get_id()));
  auto forall_v1 =
    ops->get(op(op_type::Forall, var_v1.get_id(), ex_v2.get_id()));

  std::stringstream o;
  booleguru::serialize::qcir serializer(o);
  serializer(forall_v1);

  CAPTURE(forall_v1);
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14 6
forall(1)
exists(2, 3)
output(9)
4 = and(1, 2)
8 = and(-1, -2, 3)
9 = or(4, 8)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Serialize simple non prenex CNF example to QCIR formula cleansed") {
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
forall(3)
output(8)
5 = and(1, 2, 3)
7 = exists(1, 2; 5)
8 = or(3, 7)
)";

  REQUIRE(o.str() == expected);
}

TEST_CASE("Non prenex non CNF to QCIR") {
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

   const char* expected = R"(#QCIR-14
  output(7)
  3 = xor(1, 2)
  4 = exists(1; 3)
  5 = xor(2, 4)
  6 = and(4, 5)
  7 = forall(2; 6)
  )";

  REQUIRE(o.str() == expected);

}
