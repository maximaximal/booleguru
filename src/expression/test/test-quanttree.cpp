#include <fstream>

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

  quanttree::entry& entry = t[a];
  REQUIRE(t.index(entry) == a);

  uint32_t b = t.add(Exists, 10);
  uint32_t c = t.add(Exists, 3);
  c = t.add(Forall, 2, c);
  uint32_t f = t.add(a, c);
  f = t.add(f, b);
  c = t.add(Exists, 1, f);

  t.activate_animation("EupAdown");

  t.prenex(c, &quanttree::should_inline_EupAdown);
}

TEST_CASE("Build a quanttree and erase a path below a fork") {
  quanttree t;
  uint32_t a, b, f, c;
  a = t.add(Exists, 21);
  b = t.add(Forall, 31);
  f = t.add(a, b);
  c = t.add(Exists, 11, f);

  t.remove_entry(b);

  REQUIRE(t[c].is_path());
  REQUIRE(t[t[c].p.next].is_path());
  REQUIRE(t[t[c].p.next].p.var == 21);
}

TEST_CASE("Build a quanttree and flip a part of its quantifiers") {
  quanttree t;
  uint32_t a, b, f, c;
  a = t.add(Exists, 21);
  b = t.add(Forall, 31);
  f = t.add(a, b);
  c = t.add(Exists, 11, f);

  REQUIRE(t[a].p.type == Exists);
  REQUIRE(t[b].p.type == Forall);
  REQUIRE(t[c].p.type == Exists);

  t.flip_downwards(f);

  REQUIRE(t[a].p.type == Forall);
  REQUIRE(t[b].p.type == Exists);
  REQUIRE(t[c].p.type == Exists);
}
