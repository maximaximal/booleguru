#include <booleguru/expression/op_manager.hpp>
#include <booleguru/serialize/qcir.hpp>

#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/eliminate_implication.hpp>

#include <algorithm>
#include <vector>

#include <iostream>

using namespace booleguru::expression;

namespace booleguru::serialize {

void
qcir::walk_quant(op_ref o) {
  if(o->mark)
    return;
  o->mark = true;

  std::string_view qtext = o->type == op_type::Forall ? "forall" : "exists";
  op_type t = o->type;

  bool first = true;
  if(!on_quant_prefix_) {
    if(!dry_walk_)
      o_ << o.get_id() << " = ";
    else
      ++number_of_variables_;
  }
  while(o->type == t) {
    auto v = o.get_mgr().get(op(op_type::Var, o->quant.v, 0)).get_id();
    if(first) {
      if(!dry_walk_)
        o_ << qtext << "(" << v;
      else
        ++number_of_variables_;
      first = false;
    } else {
      if(!dry_walk_)
        o_ << ", " << v;
      else
        ++number_of_variables_;
    }
    o = o.get_mgr()[o->quant.e];
  }

  bool invert = false;
  while(o->type == op_type::Not) {
    o = o.get_mgr()[o->un.c];
    invert = !invert;
  }
  if(!on_quant_prefix_) {
    if(!dry_walk_)
      o_ << "; " << (invert ? "-" : "") << o.get_id();
  }
  if(!dry_walk_)
    o_ << ")\n";

  if(on_quant_prefix_) {
    if(o->type != op_type::Forall && o->type != op_type::Exists) {
      on_quant_prefix_ = false;
      int32_t id = o.get_id();
      if(invert) {
        id = -id;
      }
      if(!dry_walk_)
        o_ << "output(" << id << ")\n";
    }
  }

  walk(o);
}

std::vector<int32_t>
qcir::walk_nargsop(std::string_view gatetype, op_ref o, bool last) {
  if(o->mark)
    return {};
  o->mark = true;
  op_type t = o->type;

  std::vector<int32_t> ops;

  auto left = o.left();
  if(left->type == t) {
    ops = walk_nargsop(gatetype, left, false);
  } else {
    bool invert = false;
    while(left->type == op_type::Not) {
      left = left.left();
      invert = !invert;
    }
    int32_t id = left.get_id();
    if(invert)
      id = -id;
    ops.push_back(id);
    walk(left);
  }
  auto right = o.right();
  if(right->type == t) {
    auto sub_ops = walk_nargsop(gatetype, right, false);
    ops.insert(ops.end(), sub_ops.begin(), sub_ops.end());
  } else {
    bool invert = false;
    while(right->type == op_type::Not) {
      right = right.left();
      invert = !invert;
    }
    int32_t id = right.get_id();
    if(invert)
      id = -id;
    ops.push_back(id);
    walk(right);
  }

  if(last) {
    if(dry_walk_) {
      ++number_of_variables_;
      return {};
    } else {
      assert(ops.size() > 1);
      o_ << o.get_id() << " = " << gatetype << "(" << ops.front();
      for(size_t i = 1; i < ops.size(); ++i) {
        o_ << ", " << ops[i];
      }
      o_ << ")\n";

      return {};
    }
  } else {
    return ops;
  }
}

void
qcir::walk_not(op_ref o) {
  bool invert = false;
  while(o->type == op_type::Not) {
    o = o.left();
    invert = !invert;
  }

  if(on_quant_prefix_ && !dry_walk_) {
    on_quant_prefix_ = false;
    int32_t id = o.get_id();
    if(invert)
      id = -id;
    o_ << "output(" << id << ")\n";
  }

  walk(o);
}
void
qcir::walk_equi(op_ref o) {
  (void)o;
  // Never happens, is transformed out.
  assert(false);
}
void
qcir::walk_impl(op_ref o) {
  (void)o;
  // Never happens, is transformed out.
  assert(false);
}
void
qcir::walk_lpmi(op_ref o) {
  (void)o;
  // Never happens, is transformed out.
  assert(false);
}
void
qcir::walk_and(op_ref o) {
  if(on_quant_prefix_ && !dry_walk_) {
    on_quant_prefix_ = false;
    o_ << "output(" << o.get_id() << ")\n";
  }
  walk_nargsop("and", o);
}
void
qcir::walk_or(op_ref o) {
  if(on_quant_prefix_ && !dry_walk_) {
    on_quant_prefix_ = false;
    o_ << "output(" << o.get_id() << ")\n";
  }
  walk_nargsop("or", o);
}
void
qcir::walk_xor(op_ref o) {
  if(o->mark)
    return;
  o->mark = true;

  auto w = [this](op_ref side) -> int32_t {
    bool invert = false;
    while(side->type == op_type::Not) {
      side = side.left();
      invert = !invert;
    }
    int32_t id = side.get_id();
    if(invert)
      id = -id;
    walk(side);
    return id;
  };
  int32_t left = w(o.left());
  int32_t right = w(o.right());
  if(dry_walk_)
    ++number_of_variables_;
  else
    o_ << o.get_id() << " = xor(" << left << ", " << right << ")\n";
}
void
qcir::walk_var(op_ref o) {
  (void)o;
}

void
qcir::operator()(expression::op_ref op) {
  op =
    transform::eliminate_implication()(transform::eliminate_equivalence()(op));

  op.get_mgr().unmark();

  std::vector<uint32_t> vars;
  std::vector<uint32_t> quantified_vars;
  auto visit = [&quantified_vars, &vars](uint32_t id,
                                         expression::op& op) -> void {
    (void)id;
    op.mark = true;
    switch(op.type) {
      case expression::op_type::Var:
        vars.push_back(op.var.v);
        break;
      case expression::op_type::Exists:
      case expression::op_type::Forall:
        quantified_vars.push_back(op.quant.v);
        break;
      default:
        break;
    }
  };
  op.get_mgr().traverse_unmarked_depth_first_through_tree(op.get_id(), visit);

  std::sort(vars.begin(), vars.end());
  std::sort(quantified_vars.begin(), quantified_vars.end());

  std::vector<uint32_t> unquantified_vars;
  std::set_difference(vars.begin(),
                      vars.end(),
                      quantified_vars.begin(),
                      quantified_vars.end(),
                      std::back_inserter(unquantified_vars));

  auto var_to_op_id = [&op](uint32_t varid) -> uint32_t {
    return op.get_mgr().get(expression::op(op_type::Var, varid, 0)).get_id();
  };

  // Using mark user flag for marking visited nodes.
  op.get_mgr().unmark();

  dry_walk_ = true;
  number_of_variables_ = 0;
  on_quant_prefix_ = true;
  walk(op);
  dry_walk_ = false;

  op.get_mgr().unmark();
  on_quant_prefix_ = true;

  o_ << "#QCIR-G14 " << number_of_variables_ << "\n";

  if(!unquantified_vars.empty()) {
    o_ << "free(";
    auto v = unquantified_vars.begin();
    o_ << var_to_op_id(*v);
    ++v;
    for(; v != unquantified_vars.end(); ++v) {
      o_ << ", " << var_to_op_id(*v);
    }
    o_ << ")\n";
  }

  walk(op);
}
}