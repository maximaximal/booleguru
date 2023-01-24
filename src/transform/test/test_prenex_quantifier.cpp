#include <sstream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <booleguru/expression/var_manager.hpp>

#include <booleguru/transform/prenex_quantifiers.hpp>

using namespace booleguru::expression;
using namespace booleguru::transform;

TEST_CASE("Transform a simple Non-Prenex formula into prenex") {
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
  auto op_ = op_g1 || ops->get(op(op_type::Exists, op_v1.get_id(), op_g2.get_id()));

  auto ex_v3 =
    ops->get(op(op_type::Exists, var_v3.get_id(), op_.get_id()));
  auto ex_v2 =
    ops->get(op(op_type::Exists, var_v2.get_id(), ex_v3.get_id()));
  auto forall_v1 =
    ops->get(op(op_type::Forall, var_v1.get_id(), ex_v2.get_id()));

  std::stringstream serialized;
  serialized << forall_v1;

  CAPTURE(serialized.str());

  prenex_quantifier p;
  p(forall_v1);

  std::stringstream transformed;
  transformed << p(forall_v1);

  CAPTURE(transformed.str());

  const char* expected = R"(some formula
)";

  REQUIRE(transformed.str() == expected);
}
