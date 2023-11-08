#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/quanttree.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/transform/prenex_quantifiers.hpp>
#include <booleguru/util/reverse.hpp>
#include <booleguru/util/unsupported.hpp>

#include <algorithm>
#include <iosfwd>
#include <iterator>
#include <limits>
#include <stack>

#include <fmt/format.h>

// Enable printing the steps of merging the op tree into the quanttree object.
// Should be deactivated pretty much always as a comment.

// #define PRINT_STEPS

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
  expression::quanttree qt;

  quanttree::should_inline_checker checker;

  std::stack<uint32_t> qts;

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

  op_id qt_root = i_->qts.top();
  i_->qts.pop();

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
      i_->qts.push(std::numeric_limits<uint32_t>::max());
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

  // The first is the child.
  uint32_t c = i_->qts.top();
  i_->qts.pop();

  // The second is always max, as it is a variable. Currently, there is no way
  // to stop the generic tree traversal. This is a possible small optimization.
  assert(i_->qts.top() == std::numeric_limits<uint32_t>::max());
  i_->qts.pop();

  encountered_quant_ = true;

  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

  uint32_t bound = old_v_obj.counter++;

  // Whenever a variable is quantified, it is bound to a new unique number (per
  // variable). Unbound variables are free, i.e. they have never been quantified
  // before.
  //
  // Variables only have to actually be replaced, if they are bound more than
  // once. Otherwise, they are just used as-is. This speeds up well-formed
  // formulas without overlapping variables. Rebinding variables is very
  // expensive because of the required full formula traversal, so it is
  // desirable to avoid this operation.

  if(bound > 0) {
    auto bound_v = o.get_mgr().get(
      expression::op(expression::op_type::Var, old_v.v, bound, old_v.i));
    o = rebind_variable(o, bound_v, c);
  } else {
    uint32_t user_int32 = i_->qt.add((expression::op_type)o->type,
                                     static_cast<uint32_t>(o->quant.v),
                                     static_cast<uint32_t>(c));

#ifdef PRINT_STEPS
    fmt::println("Quant with bound==0 {}: quantify {} over {} together to {}",
                 op_type_to_str(o->type),
                 o.left().to_string(),
                 o.right().to_string(),
                 user_int32);
#endif
    o = o.right();

    i_->qts.push(user_int32);
  }

  // This invariant has to hold in order for the value carrying through the
  // int32_t field of an op to be valid.
  static_assert(std::numeric_limits<uint32_t>::max()
                == static_cast<uint32_t>(
                  static_cast<int32_t>(std::numeric_limits<uint32_t>::max())));

  return o;
}

expression::op_ref
prenex_quantifier::rebind_variable(expression::op_ref o,
                                   expression::op_ref bound_v,
                                   uint32_t c) {
  op_ref e = o.right();
  uint32_t user_int32 = i_->qt.add((expression::op_type)o->type,
                                   static_cast<uint32_t>(bound_v.get_id()),
                                   static_cast<uint32_t>(c));

#ifdef PRINT_STEPS
  fmt::println("Quant {}: quantify {} over {} together to {}",
               op_type_to_str(o->type),
               bound_v.to_string(),
               e.to_string(),
               user_int32);
#endif

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
  e = new_e_op;

  i_->qts.push(user_int32);

  return e;
}

expression::op_ref
prenex_quantifier::walk_not(expression::op_ref o) {
  uint32_t c = i_->qts.top();
  if(c != std::numeric_limits<uint32_t>::max()) {
#ifdef PRINT_STEPS
    fmt::println(
      "Flip op {} which is {} in the quanttree downwards", o.to_string(), c);
#endif
    i_->qt.flip_downwards(static_cast<uint32_t>(c));
  }
  return o;
}

// This eliminates impls of the form ->
expression::op_ref
prenex_quantifier::walk_impl(expression::op_ref o) {
  auto lright = i_->qts.top();
  i_->qts.pop();
  auto lleft = i_->qts.top();
  i_->qts.pop();

  op_ref left
    = o.get_mgr().get(expression::op(expression::op_type::Not, o->left(), 0));

  if(lleft != std::numeric_limits<uint32_t>::max())
    i_->qt.flip_downwards(static_cast<uint32_t>(lleft));

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), o->right()));
  i_->qts.push(i_->qt.add(lleft, lright));
  return impl;
}

// This eliminates impls of the form <-
expression::op_ref
prenex_quantifier::walk_lpmi(expression::op_ref o) {
  auto lright = i_->qts.top();
  i_->qts.pop();
  auto lleft = i_->qts.top();
  i_->qts.pop();

  op_ref right
    = o.get_mgr().get(expression::op(expression::op_type::Not, o->right(), 0));

  if(lright != std::numeric_limits<uint32_t>::max())
    i_->qt.flip_downwards(lright);

  op_ref impl = o.get_mgr().get(
    expression::op(expression::op_type::Or, o->left(), right.get_id()));
  i_->qts.push(i_->qt.add(lleft, lright));
  return impl;
}

expression::op_ref
prenex_quantifier::walk_equi(expression::op_ref o) {
  (void)o;
  throw util::unsupported("equi unsupported for prenexing");
}

expression::op_ref
prenex_quantifier::walk_bin(expression::op_ref o) {
  auto right = i_->qts.top();
  i_->qts.pop();
  auto left = i_->qts.top();
  i_->qts.pop();
  uint32_t res = i_->qt.add(left, right);

#ifdef PRINT_STEPS
  fmt::println(
    "BinOp {}: add {} and {} together to {}", o.to_string(), left, right, res);
#endif

  i_->qts.push(res);
  return o;
}

expression::op_ref
prenex_quantifier::walk_xor(expression::op_ref o) {
  (void)o;
  throw util::unsupported("xor unsupported for prenexing");
}
}
