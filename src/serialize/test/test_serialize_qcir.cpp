#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/serialize/qcir.hpp>

#include <booleguru/expression/var_manager.hpp>

using namespace booleguru::serialize;
using namespace booleguru::expression;

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

  const char* expected = R"(#QCIR-G14
forall(3)
output(8)
7 = exists(0, 1; 5)
5 = and(0, 1, 2)
8 = and(3, 7)
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
  CAPTURE(o.str());

  const char* expected = R"(#QCIR-G14
forall(3)
output(-9)
7 = exists(0, 1; 5)
5 = and(0, 1, 2)
9 = and(3, -7)
)";

  REQUIRE(o.str() == expected);
}
