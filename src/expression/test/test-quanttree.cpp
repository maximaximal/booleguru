#include <catch2/catch_test_macros.hpp>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/quanttree.hpp>

using namespace booleguru::expression;
using namespace booleguru::expression::literals;
using enum op_type;

#include <iostream>
using std::cout;
using std::endl;

TEST_CASE("Build a quanttree and call Eup Aup") {
  quanttree t;
  uint32_t a;
  a = t.add(Exists, 21);
  a = t.add(Forall, 20, a);
  uint32_t b = t.add(Exists, 10);
  uint32_t c = t.add(Exists, 3);
  c = t.add(Forall, 2, c);
  uint32_t f = t.add(a, c);
  f = t.add(f, b);
  c = t.add(Exists, 1, f);

  quanttree::quantvec critical = t.compute_critical_path(c);

  t.to_dot(cout, critical);

  quanttree::quantvec eup_aup = t.Eup_Aup(critical);
}
