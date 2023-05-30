#include <booleguru/transform/prenex_quantifiers.hpp>

#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/quanttree.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <iostream>
#include <iterator>
#include <limits>

using std::cout;
using std::endl;

namespace booleguru::transform {

#define IT(I) ops[I->var] << ":" << *I

using namespace expression;

static quanttree::should_inline_checker checkermap[4] = {
  &quanttree::should_inline_EupAup,
  &quanttree::should_inline_EupAdown,
  &quanttree::should_inline_EdownAup,
  &quanttree::should_inline_EdownAdown,
};

struct prenex_quantifier::inner {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  expression::quanttree qt;
  uint32_t qt_root = 0;

  // The stack of quantifiers is seen as in a stackful virtual machine. Binops
  // take two and produce one, quantops add one for each started quantifier run,
  // until a .
  std::stack<uint32_t> qt_stack;

  quanttree::should_inline_checker checker;

  inner(prenex_quantifier::kind k)
    : checker(checkermap[k]) {}
};

prenex_quantifier::prenex_quantifier(kind k)
  : i_(std::make_unique<inner>(k)) {}

prenex_quantifier::~prenex_quantifier() = default;

op_ref
prenex_quantifier::operator()(op_ref o) {
  using enum op_type;

  // I want to postorder traverse the tree and remove all quantifiers in the
  // process. In the end, the collected quantifiers are used from the quanttree.

  uint32_t new_root = o.get_mgr().traverse_postorder_with_stack(
    o.get_id(), [this](expression::op_manager* ops, uint32_t o) -> uint32_t {
      return walk((*ops)[o]).get_id();
    });

  i_->qt.prenex(i_->qt_root, i_->checker);
  return i_->qt.prepend_marked_to_op(i_->qt_root, o.get_mgr()[new_root]);
}

expression::op_ref
prenex_quantifier::walk(expression::op_ref o) {
  using enum expression::op_type;
  switch(o->type) {
    case Forall:
      [[fallthrough]];
    case Exists:
      return walk_quant(o);
    case Var:
      return walk_var(o);
    case Not:
      return walk_not(o);
    case Impl:
      return walk_impl(o);
    case Lpmi:
      return walk_lpmi(o);
    case Equi:
      return walk_equi(o);
    case Or:
      [[fallthrough]];
    case And:
      return walk_bin(o);
    case Xor:
      return walk_xor(o);
    default:
      return o;
  }
}

expression::op_ref
prenex_quantifier::walk_quant(expression::op_ref o) {
  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

  uint32_t outer_bound = i_->bounds_map[old_v.v];
  uint32_t bound = old_v_obj.counter++;
  i_->bounds_map[old_v.v] = bound;

  auto bound_v =
    o.get_mgr().get(expression::op(expression::op_type::Var, old_v.v, bound));

  op_ref e = o.right();
  e->user_int32 = i_->qt.add((expression::op_type)o->type,
                             bound_v.get_id(),
                             static_cast<uint32_t>(e->user_int32));

  i_->bounds_map[old_v.v] = outer_bound;

  // This invariant has to hold in order for the value carrying through the
  // int32_t field of an op to be valid.
  static_assert(std::numeric_limits<uint32_t>::max() ==
                static_cast<uint32_t>(
                  static_cast<int32_t>(std::numeric_limits<uint32_t>::max())));

  return e;
}

expression::op_ref
prenex_quantifier::walk_var(expression::op_ref o) {
  auto it = i_->bounds_map.find(o->var.v);
  if(it != i_->bounds_map.end()) {
    o = o.get_mgr().get(
      expression::op(expression::op_type::Var, o->var.v, it->second));
  }
  o->user_int32 = static_cast<int32_t>(std::numeric_limits<uint32_t>::max());
  return o;
}

expression::op_ref
prenex_quantifier::walk_not(expression::op_ref o) {
  o->user_int32 = o.get_mgr().getobj(o->un.c).user_int32;
  i_->qt.flip_downwards(static_cast<uint32_t>(o->user_int32));
  return o;
}

// This eliminates impls of the form ->
expression::op_ref
prenex_quantifier::walk_impl(expression::op_ref o) {
  op_ref left =
    o.get_mgr().get(expression::op(expression::op_type::Not, o->left(), 0));
  left->user_int32 = o.left()->user_int32;
  i_->qt.flip_downwards(static_cast<uint32_t>(left->user_int32));

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), o->right()));
  impl->user_int32 = i_->qt.add(static_cast<uint32_t>(left->user_int32),
                                static_cast<uint32_t>(o.right()->user_int32));
  return impl;
}

// This eliminates impls of the form <-
expression::op_ref
prenex_quantifier::walk_lpmi(expression::op_ref o) {
  op_ref right =
    o.get_mgr().get(expression::op(expression::op_type::Not, o->right(), 0));
  right->user_int32 = o.right()->user_int32;
  i_->qt.flip_downwards(static_cast<uint32_t>(right->user_int32));

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, o->left(), right.get_id()));
  impl->user_int32 = i_->qt.add(static_cast<uint32_t>(o.left()->user_int32),
                                static_cast<uint32_t>(right->user_int32));
  return impl;
}

expression::op_ref
prenex_quantifier::walk_equi(expression::op_ref o) {
  return o;
}

expression::op_ref
prenex_quantifier::walk_bin(expression::op_ref o) {
  o->user_int32 = i_->qt.add(static_cast<uint32_t>(o.left()->user_int32),
                             static_cast<uint32_t>(o.right()->user_int32));
  return o;
}

expression::op_ref
prenex_quantifier::walk_xor(expression::op_ref o) {
  return o;
}
}
