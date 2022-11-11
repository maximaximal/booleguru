#include <booleguru/expression/op_manager.hpp>
#include <booleguru/serialize/qcir.hpp>

using namespace booleguru::expression;

namespace booleguru::serialize {

void
qcir::walk_quant(op_ref o) {
  std::string_view qtext = o->type == op_type::Forall ? "forall" : "exists";
  op_type t = o->type;

  bool first = true;
  if(!on_quant_prefix_) {
    o_ << o.get_id() << " = ";
  }
  while(o->type == t) {
    auto v = o.get_mgr().get(op(op_type::Var, o->quant.v, 0)).get_id();
    if(first) {
      o_ << qtext << "(" << v;
      first = false;
    } else {
      o_ << ", " << v;
    }
    o = o.get_mgr()[o->quant.e];
  }

  bool had_invert = false;
  bool invert = false;
  while(o->type == op_type::Not) {
    o = o.get_mgr()[o->un.c];
    invert = !invert;
    had_invert = true;
  }
  if(!on_quant_prefix_) {
    o_ << "; " << (invert ? "-" : "") << o.get_id();
  }
  o_ << ")\n";

  if(on_quant_prefix_) {
    if(o->type != op_type::Forall && o->type != op_type::Exists) {
      on_quant_prefix_ = false;
      int32_t id = o.get_id();
      if(invert) {
        id = -id;
      }
      o_ << "output(" << id << ")\n";
    }
  }

  walk(o);
}

std::vector<int32_t>
qcir::walk_nargsop(std::string_view gatetype, op_ref o, bool last) {
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
    assert(ops.size() > 1);
    o_ << o.get_id() << " = " << gatetype << "(" << ops.front();
    for(size_t i = 1; i < ops.size(); ++i) {
      o_ << ", " << ops[i];
    }
    o_ << ")\n";

    return {};
  } else {
    return ops;
  }
}

void
qcir::walk_not(op_ref o) {
  // Should never occur!
  assert(false);
}
void
qcir::walk_equi(op_ref o) {}
void
qcir::walk_impl(op_ref o) {}
void
qcir::walk_lpmi(op_ref o) {}
void
qcir::walk_xor(op_ref o) {}
void
qcir::walk_var(op_ref o) {}

void
qcir::operator()(expression::op_ref op) {
  o_ << "#QCIR-G14\n";
  walk(op);
}
}
