#include <booleguru/transform/prenex_quantifiers.hpp>

#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/quanttree.hpp>
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
  uint32_t root;
};

prenex_quantifier::prenex_quantifier()
  : i_(std::make_unique<inner>()) {}

op_ref
prenex_quantifier::operator()(op_ref o) {
  using enum op_type;

  // I want to postorder traverse the tree and remove all quantifiers in the
  // process. In the end, the collected quantifiers are used from the quanttree.

  struct e {
    op_ref o;
    uint32_t e;
  };

  std::vector<op_ref> op_stack{ o };

  while(!op_stack.empty()) {
    op_ref t = op_stack.back();
    switch(t->type) {
      case Forall:
      case Exists: {
        break;
      }

      case None:
      default:
        break;
    }
  }

  i_->qt.prenex(i_->root, *i_);
  return i_->qt.prepend_marked_to_op(i_->root, o);
}

prenex_quantifier::walker_action
prenex_quantifier::walk_quant(expression::op_ref o) {
  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

  uint32_t outer_bound = bounds_map[old_v.v];
  uint32_t bound = old_v_obj.counter++;
  bounds_map[old_v.v] = bound;

  auto bound_v =
    o.get_mgr().get(expression::op(expression::op_type::Var, old_v.v, bound));

  auto e = this->visit(o.get_mgr()[o->quant.e]);
  strategy_->child = strategy_->qt.add(
    (expression::op_type)o->type, bound_v.get_id(), strategy_->child);

  bounds_map[old_v.v] = outer_bound;

  return e;
}

expression::op_ref
prenex_quantifier::walk_exists(expression::op_ref o) {
  return walk_quant(o);
}

expression::op_ref
prenex_quantifier::walk_forall(expression::op_ref o) {
  return walk_quant(o);
}

expression::op_ref
prenex_quantifier::walk_var(expression::op_ref o) {
  auto it = i_->bounds_map.find(o->var.v);
  if(it != i_->bounds_map.end()) {
    return o.get_mgr().get(
      expression::op(expression::op_type::Var, o->var.v, it->second));
  }
  return o;
}

expression::op_ref
prenex_quantifier::walk_not(expression::op_ref o) {
  auto ctx = i_->qt.open_flip_ctx();
  return o;
}

// This eliminates impls of the form ->
template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_impl(expression::op_ref o) {
  auto begin_before_visit = quant_stack.begin();
  auto left = !this->visit(o.left());
  auto begin_after_visit = quant_stack.begin();
  auto right = this->visit(o.right());
  if(begin_before_visit != begin_after_visit) {
    for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
      expression::op_type& t = it->t;
      t = expression::op_type_flip_quantifier(t);
    }
  }
  return o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
}

// This eliminates impls of the form <-
template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_lpmi(expression::op_ref o) {
  auto left = this->visit(o.left());
  auto begin_before_visit = quant_stack.begin();
  auto right = !this->visit(o.right());
  auto begin_after_visit = quant_stack.begin();
  if(begin_before_visit != begin_after_visit) {
    for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
      expression::op_type& t = it->t;
      t = expression::op_type_flip_quantifier(t);
    }
  }
  return o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
}

template struct prenex_quantifier<prenex_quantifier_Eup_Aup>;
template struct prenex_quantifier<prenex_quantifier_Eup_Adown>;
template struct prenex_quantifier<prenex_quantifier_Edown_Aup>;
template struct prenex_quantifier<prenex_quantifier_Edown_Adown>;
}

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e) {
  char leaf = e.subtree_leaf ? 'L' : 'I';
  return o << e.t << ":" << e.nesting << leaf;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::list<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}
