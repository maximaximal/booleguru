#pragma once

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
using expression::op_ref;

static std::pair<int32_t, int32_t>
compute_number_of_clauses_and_variables_in_marked(const expression::op_ref& o,
                                                  auto o_,
                                                  bool mappings) {
  const expression::op_manager& mgr = o.get_mgr();

  int32_t num_clauses = 1;
  int32_t num_variables = 0;

  bool insert_top = false;
  bool insert_bottom = false;

  for(size_t i = 0; i < mgr.size(); ++i) {
    const auto& e = mgr.objects()[i];
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
        if(op.var.v == expression::var_manager::LITERAL_TOP) {
          insert_top = true;
        } else if(op.var.v == expression::var_manager::LITERAL_BOTTOM) {
          insert_bottom = true;
        }
        if(mappings)
          o_.insert_mapping_comment(op.user_int32, i + 1, o.get_mgr());
        break;
      case expression::op_type::None:
        [[fallthrough]];
      case expression::op_type::Forall:
        [[fallthrough]];
      case expression::op_type::Exists:
        break;
    }
  }

  if(insert_top)
    ++num_clauses;

  if(insert_bottom)
    ++num_clauses;

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
  if constexpr(std::is_same_v<id, int32_t>) {
    auto [num_clauses, num_variables]
      = compute_number_of_clauses_and_variables_in_marked(
        o, o_, mapping_comments_);
    clauses_ = num_clauses;
    variables_ = num_variables;
    o_.problem(num_variables, num_clauses);
  }

  // Input quantifiers
  expression::op_ref q = o;
  while(q->is_quant()) {
    assert(q.left()->type == expression::op_type::Var);
    const expression::op& q_ = q.get_obj();
    uint32_t left_id = static_cast<uint32_t>(q_.left());
    const expression::op& left = q.get_mgr().getobj(left_id);
    assert(left_id != 0);
    assert(left.type == expression::op_type::Var);
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

  uint32_t top_id = 0;
  uint32_t bottom_id = 0;

  for(size_t i = 0; i < mgr.size(); ++i) {
    auto& e = mgr.objects()[i];
    const expression::op& op = e.first;
    id t = o_.op_ref_to_ref(op, e.second);

    if(!op.mark)
      continue;

    switch(op.type) {
      case expression::op_type::Equi: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(t, o_.not_op(l), o_.not_op(r));
        o_.ternary(t, l, r);
        o_.ternary(o_.not_op(t), o_.not_op(l), r);
        o_.ternary(o_.not_op(t), l, o_.not_op(r));
        break;
      }
      case expression::op_type::Xor: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(o_.not_op(t), l, r);
        o_.ternary(t, o_.not_op(l), r);
        o_.ternary(t, l, o_.not_op(r));
        o_.ternary(o_.not_op(t), o_.not_op(l), o_.not_op(r));
        break;
      }
      case expression::op_type::Or: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(l, r, o_.not_op(t));
        o_.binary(o_.not_op(l), t);
        o_.binary(o_.not_op(r), t);
        break;
      }
      case expression::op_type::And: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.ternary(o_.not_op(l), o_.not_op(r), t);
        o_.binary(l, o_.not_op(t));
        o_.binary(r, o_.not_op(t));

        break;
      }
      case expression::op_type::Impl: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.binary(t, l);
        o_.binary(t, o_.not_op(r));
        o_.ternary(o_.not_op(t), o_.not_op(l), r);
        break;
      }
      case expression::op_type::Lpmi: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        id r = o_.op_ref_to_ref(mgr.getobj(op.right()), op.right());
        o_.binary(t, o_.not_op(l));
        o_.binary(t, r);
        o_.ternary(o_.not_op(t), l, o_.not_op(r));
        break;
      }
      case expression::op_type::Not: {
        id l = o_.op_ref_to_ref(mgr.getobj(op.left()), op.left());
        o_.binary(o_.not_op(l), o_.not_op(t));
        o_.binary(l, t);
        break;
      }
      case expression::op_type::Var:
        if(op.var.v == expression::var_manager::LITERAL_TOP) {
          top_id = op.user_int32;
        }
        if(op.var.v == expression::var_manager::LITERAL_BOTTOM) {
          bottom_id = op.user_int32;
        }
        break;
      case expression::op_type::None:
        [[fallthrough]];
      case expression::op_type::Forall:
        [[fallthrough]];
      case expression::op_type::Exists:
        break;
    }
  }

  if(top_id)
    o_.unit(top_id);
  if(bottom_id)
    o_.unit(o_.not_op(bottom_id));

  o_.unit(o_.op_ref_to_ref(q.get_obj(), q.get_id()));

  return o_.get_out();
}
}
