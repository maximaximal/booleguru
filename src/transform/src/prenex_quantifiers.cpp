#include <booleguru/transform/prenex_quantifiers.hpp>

#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/quanttree.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/util/reverse.hpp>
#include <booleguru/util/unsupported.hpp>

#include <iosfwd>
#include <iostream>
#include <iterator>
#include <limits>

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

  i_->qt.set_lookup_op_manager(&o.get_mgr());

  // I want to postorder traverse the tree and remove all quantifiers in the
  // process. In the end, the collected quantifiers are used from the quanttree.

  op_id new_root = o.get_mgr().traverse_postorder_with_stack(
    o.get_id(), [this](expression::op_manager* ops, op_id o) -> op_id {
      return walk((*ops)[o]).get_id();
    });

  if(!encountered_quant_) {
    // No quantifiers have to be removed, the op is already devoid of
    // quantifiers! Can directly return the same op ref. This may not be equal
    // to new_root, as new_root may has new nodes because of resolving
    // implications.
    return o;
  }

  op_id qt_root = o.get_mgr().getobj(new_root).user_int32;

  i_->qt.prenex(static_cast<uint32_t>(qt_root), i_->checker);
  op_ref prepended = i_->qt.prepend_marked_to_op(static_cast<uint32_t>(qt_root),
                                                 o.get_mgr()[new_root]);
  return prepended;
}

void
prenex_quantifier::animate(const std::string& path) {
  i_->qt.activate_animation(path);
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
      o->user_int32 = std::numeric_limits<uint32_t>::max();
      return o;
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
  assert(o->type == expression::op_type::Forall
         || o->type == expression::op_type::Exists);
  assert(o.left()->type == expression::op_type::Var);

  encountered_quant_ = true;

  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

  uint32_t bound = ++old_v_obj.counter;
  i_->bounds_map[static_cast<uint32_t>(old_v.v)] = bound;

  // Whenever a variable is quantified, it is bound to a new unique number (per
  // variable). Unbound variables are free, i.e. they have never been quantified
  // before.

  auto bound_v = o.get_mgr().get(
    expression::op(expression::op_type::Var, old_v.v, bound, old_v.i));
  bound_v->user_int32
    = static_cast<uint32_t>(std::numeric_limits<uint32_t>::max());

  op_ref e = o.right();
  uint32_t user_int32 = i_->qt.add((expression::op_type)o->type,
                                   static_cast<uint32_t>(bound_v.get_id()),
                                   static_cast<uint32_t>(e->user_int32));

  // Travese all variables downwards again, now that the bound is fixed.
  op_id new_e = o.get_mgr().traverse_postorder_with_stack(
    e.get_id(), [bound_v](op_manager* ops, op_id id) -> op_id {
      op_ref o = (*ops)[id];
      switch(o->type) {
        case op_type::Var:
          if(o->var.v == bound_v->var.v && !o->var.q) {
            // Replace the unbound variable with the bound variant.
            return bound_v.get_id();
          }
          return id;
        default:
          return id;
      }
    });

  op_ref new_e_op = o.get_mgr()[new_e];
  new_e_op->user_int32 = user_int32;
  e = new_e_op;

  // This invariant has to hold in order for the value carrying through the
  // int32_t field of an op to be valid.
  static_assert(std::numeric_limits<uint32_t>::max()
                == static_cast<uint32_t>(
                  static_cast<int32_t>(std::numeric_limits<uint32_t>::max())));

  return e;
}

expression::op_ref
prenex_quantifier::walk_not(expression::op_ref o) {
  o->user_int32 = o.get_mgr().getobj(o->un.c).user_int32;
  if(static_cast<uint32_t>(o->user_int32)
     != std::numeric_limits<uint32_t>::max())
    i_->qt.flip_downwards(static_cast<uint32_t>(o->user_int32));
  return o;
}

// This eliminates impls of the form ->
expression::op_ref
prenex_quantifier::walk_impl(expression::op_ref o) {
  op_ref left
    = o.get_mgr().get(expression::op(expression::op_type::Not, o->left(), 0));

  if(static_cast<uint32_t>(o.left()->user_int32)
     != std::numeric_limits<uint32_t>::max())
    i_->qt.flip_downwards(static_cast<uint32_t>(o.left()->user_int32));

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), o->right()));
  impl->user_int32 = i_->qt.add(static_cast<uint32_t>(o.left()->user_int32),
                                static_cast<uint32_t>(o.right()->user_int32));
  return impl;
}

// This eliminates impls of the form <-
expression::op_ref
prenex_quantifier::walk_lpmi(expression::op_ref o) {
  op_ref right
    = o.get_mgr().get(expression::op(expression::op_type::Not, o->right(), 0));

  if(static_cast<uint32_t>(o.right()->user_int32)
     != std::numeric_limits<uint32_t>::max())
    i_->qt.flip_downwards(static_cast<uint32_t>(o.right()->user_int32));

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, o->left(), right.get_id()));
  impl->user_int32 = i_->qt.add(static_cast<uint32_t>(o.left()->user_int32),
                                static_cast<uint32_t>(o.right()->user_int32));
  return impl;
}

expression::op_ref
prenex_quantifier::walk_equi(expression::op_ref o) {
  throw util::unsupported("equi unsupported for prenexing");
}

expression::op_ref
prenex_quantifier::walk_bin(expression::op_ref o) {
  o->user_int32 = i_->qt.add(static_cast<uint32_t>(o.left()->user_int32),
                             static_cast<uint32_t>(o.right()->user_int32));
  return o;
}

expression::op_ref
prenex_quantifier::walk_xor(expression::op_ref o) {
  throw util::unsupported("xor unsupported for prenexing");
  return o;
}
}
