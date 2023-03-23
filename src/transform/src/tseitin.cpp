#include <cstdint>

#include <booleguru/transform/tseitin.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <stdexcept>
#include <type_traits>

#include <iostream>

#include "booleguru/transform/output_to_op.hpp"
#include "booleguru/transform/output_to_qdimacs.hpp"

namespace booleguru::transform {
static std::pair<int32_t, int32_t>
compute_number_of_clauses_and_variables_in_marked(const expression::op_ref& o) {
  const expression::op_manager& mgr = o.get_mgr();

  int32_t num_clauses = 1;
  int32_t num_variables = 0;

  for(const auto& e : mgr.objects()) {
    const expression::op& op = e.first;

    if(!op.mark)
      continue;

    switch(op.type) {
      case expression::op_type::Equi:
        [[fallthrough]];
      case expression::op_type::Xor:
        num_clauses += 4;
        ++num_variables;
        op.user_int32 = num_variables;
        break;
      case expression::op_type::Or:
        [[fallthrough]];
      case expression::op_type::And:
        [[fallthrough]];
      case expression::op_type::Impl:
        [[fallthrough]];
      case expression::op_type::Lpmi:
        num_clauses += 3;
        ++num_variables;
        op.user_int32 = num_variables;
        break;
      case expression::op_type::Not:
        num_clauses += 2;
        ++num_variables;
        op.user_int32 = num_variables;
        break;
      case expression::op_type::Var:
        ++num_variables;
        op.user_int32 = num_variables;
        break;
      case expression::op_type::None:
        [[fallthrough]];
      case expression::op_type::Forall:
        [[fallthrough]];
      case expression::op_type::Exists:
        break;
    }
  }

  return std::make_pair(num_clauses, num_variables);
}

template<class O>
typename tseitin<O>::TransformResult
tseitin<O>::operator()(expression::op_ref o) {
  expression::op_manager& mgr = o.get_mgr();

  assert(o->is_prenex);
  if(!o->is_prenex) {
    throw std::invalid_argument(
      "Op is required to be in prenex form for tseitin transformation!");
  }

  // Mark all nodes that can be reached from the given node, so the next
  // algorithm can work without recursion.
  mgr.unmark();
  mgr.mark_through_tree(o.get_id());

  // int32_t means that references actually map to literals in a solver call /
  // QDIMACS output. This also requires the problem to be initiated.
  if constexpr(std::is_same_v<ref, int32_t>) {
    auto [num_clauses, num_variables] =
      compute_number_of_clauses_and_variables_in_marked(o);
    o_.problem(num_variables, num_clauses);
  }

  // Input quantifiers
  expression::op_ref q = o;
  while(q->is_quant()) {
    assert(q.left()->type == expression::op_type::Var);
    const expression::op& q_ = q.get_obj();
    const expression::op& left = q.left().get_obj();
    const expression::op_ref::ref left_id = q_.left();
    if(q->type == expression::op_type::Exists) {
      o_.exists(o_.op_ref_to_ref(left, left_id));
    } else {
      o_.forall(o_.op_ref_to_ref(left, left_id));
    }
    q = q.right();
  }

  if(o->is_quant()) {
    // Append existential tseitin variables.
    for(const auto& e : mgr.objects()) {
      const expression::op& op = e.first;

      if(!op.mark)
        continue;

      switch(op.type) {
        case expression::op_type::Equi:
          [[fallthrough]];
        case expression::op_type::Xor:
          [[fallthrough]];
        case expression::op_type::Or:
          [[fallthrough]];
        case expression::op_type::And:
          [[fallthrough]];
        case expression::op_type::Impl:
          [[fallthrough]];
        case expression::op_type::Lpmi:
          [[fallthrough]];
        case expression::op_type::Not:
          o_.exists(o_.op_ref_to_ref(op, e.second));
          break;
        case expression::op_type::None:
          [[fallthrough]];
        case expression::op_type::Var:
          [[fallthrough]];
        case expression::op_type::Forall:
          [[fallthrough]];
        case expression::op_type::Exists:
          break;
      }
    }
  }

  o_.end_prefix();

  for(const auto& e : mgr.objects()) {
    const expression::op& op = e.first;
    ref t = o_.op_ref_to_ref(op, e.second);

    if(!op.mark)
      continue;

    switch(op.type) {
      case expression::op_type::Equi: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(t, o_.not_op(l), o_.not_op(r));
        o_.ternary(t, l, r);
        o_.ternary(o_.not_op(t), o_.not_op(l), r);
        o_.ternary(o_.not_op(t), l, o_.not_op(r));
        break;
      }
      case expression::op_type::Xor: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(o_.not_op(t), l, r);
        o_.ternary(t, o_.not_op(l), r);
        o_.ternary(t, l, o_.not_op(r));
        o_.ternary(o_.not_op(t), o_.not_op(l), o_.not_op(r));
        break;
      }
      case expression::op_type::Or: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(l, r, o_.not_op(t));
        o_.binary(o_.not_op(l), t);
        o_.binary(o_.not_op(r), t);
        break;
      }
      case expression::op_type::And: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(o_.not_op(l), o_.not_op(r), t);
        o_.binary(l, o_.not_op(t));
        o_.binary(r, o_.not_op(t));

        break;
      }
      case expression::op_type::Impl: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.binary(t, l);
        o_.binary(t, o_.not_op(r));
        o_.ternary(o_.not_op(t), o_.not_op(l), r);
        break;
      }
      case expression::op_type::Lpmi: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        ref r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.binary(t, o_.not_op(l));
        o_.binary(t, r);
        o_.ternary(o_.not_op(t), l, o_.not_op(r));
        break;
      }
      case expression::op_type::Not: {
        ref l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        o_.binary(o_.not_op(l), o_.not_op(t));
        o_.binary(l, t);
        break;
      }
      case expression::op_type::None:
        [[fallthrough]];
      case expression::op_type::Var:
        [[fallthrough]];
      case expression::op_type::Forall:
        [[fallthrough]];
      case expression::op_type::Exists:
        break;
    }
  }

  o_.unit(o_.op_ref_to_ref(q.get_obj(), q.get_id()));

  return o_.get_out();
}

template class tseitin<output_to_qdimacs>;
template class tseitin<output_to_op>;
}
