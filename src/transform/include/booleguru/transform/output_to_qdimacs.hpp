#pragma once

#include <cassert>
#include <cstdint>
#include <ostream>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
class output_to_qdimacs {
  std::ostream& o;

  bool in_exists = false;
  bool in_forall = false;
  bool in_prefix = true;

  void inline maybe_end_exists() {
    if(in_exists) {
      o << "0\n";
      in_exists = false;
    }
  }

  void inline maybe_end_forall() {
    if(in_forall) {
      o << "0\n";
      in_forall = false;
    }
  }

  void inline maybe_end_quant() {
    assert(!(in_exists && in_forall));
    maybe_end_exists();
    maybe_end_forall();
  }

  public:
  using initarg = std::ostream&;
  using TransformResult = void;

  explicit output_to_qdimacs(initarg o)
    : o(o) {}

  void problem(int32_t variables, int32_t clauses) {
    o << "p cnf " << variables << " " << clauses << "\n";
  }

  using ref = int32_t;
  void exists(ref x) {
    assert(in_prefix);
    assert(x != 0);
    maybe_end_forall();
    if(!in_exists) {
      o << "e ";
      in_exists = true;
    }
    o << x << " ";
  }
  void forall(ref x) {
    assert(in_prefix);
    assert(x != 0);
    maybe_end_exists();
    if(!in_forall) {
      o << "a ";
      in_forall = true;
    }
    o << x << " ";
  }
  void end_prefix() {
    maybe_end_quant();
    in_prefix = false;
  };
  void unit(ref x1) {
    assert(!in_prefix);
    assert(x1 != 0);
    o << x1 << " 0\n";
  }
  void binary(ref x1, ref x2) {
    assert(!in_prefix);
    assert(x1 != 0);
    assert(x2 != 0);
    o << x1 << " " << x2 << " 0\n";
  }
  void ternary(ref x1, ref x2, ref x3) {
    assert(!in_prefix);
    assert(x1 != 0);
    assert(x2 != 0);
    assert(x3 != 0);
    o << x1 << " " << x2 << " " << x3 << " 0\n";
  }

  TransformResult get_out() const {}

  void insert_mapping_comment(ref id, std::string_view name) {
    o << "c " << id << " " << name << "\n";
  }

  inline constexpr ref op_ref_to_ref(const expression::op& o,
                                     expression::op_ref::ref id) {
    (void)id;
    return o.user_int32;
  }

  inline ref not_op(ref r) { return -r; }

  inline void serialize_cnf_op(const expression::op_ref& o) {
    using enum expression::op_type;

    assert(o->is_cnf);
    assert(o->is_prenex);
    expression::op_ref i = o;
    expression::op_manager& ops = i.get_mgr();

    std::stack<uint32_t> s;

    // Variable IDs
    s.emplace(o.get_id());
    int32_t var_id = 1;
    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& op = ops.getobj(i);
      switch(op.type) {
        case Or:
          [[fallthrough]];
        case And:
          s.emplace(op.bin.l);
          s.emplace(op.bin.r);
          break;
        case Forall:
          [[fallthrough]];
        case Exists:
          s.emplace(op.quant.e);
          break;
        case Not:
          s.emplace(op.un.c);
          break;
        case Var:
          op.user_int32 = 0;
          break;
        default:
          assert(false);
          break;
      }
    }
    s.emplace(o.get_id());
    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& op = ops.getobj(i);
      switch(op.type) {
        case Or:
          [[fallthrough]];
        case And:
          s.emplace(op.bin.l);
          s.emplace(op.bin.r);
          break;
        case Forall:
          [[fallthrough]];
        case Exists:
          s.emplace(op.quant.e);
          break;
        case Not:
          s.emplace(op.un.c);
          break;
        case Var:
          if(!op.user_int32) {
            op.user_int32 = var_id++;
            insert_mapping_comment(op.user_int32, ops[i].to_string());
          }
          break;
        default:
          assert(false);
          break;
      }
    }

    // Mappings
    while(i->is_quant()) {
      i = i.right();
    }
    uint32_t behind_prefix = i.get_id();

    int32_t variables = var_id - 1;
    int32_t clauses = 0;

    s.emplace(behind_prefix);
    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& o = ops.getobj(i);
      if(o.type == And) {
        s.emplace(o.bin.l);
        s.emplace(o.bin.r);
        ++clauses;
      }
    }

    problem(variables, clauses);
    i = o;
    while(i->is_quant()) {
      if(i->type == Exists) {
        exists(i.left()->user_int32);
      }
      if(i->type == Exists) {
        forall(i.left()->user_int32);
      }
      i = i.right();
    }

    end_prefix();

    std::stack<uint32_t> inner_s;

    s.emplace(behind_prefix);
    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      expression::op_ref o = ops[i];
      if(o->type == And) {
        if(o.left()->type == And) {
          s.emplace(o->left());
        } else {
          print_or_tree(ops, o->left(), inner_s);
        }
        if(o.right()->type == And) {
          s.emplace(o->right());
        } else {
          print_or_tree(ops, o->right(), inner_s);
        }
      }
    }
  }

  inline void print_or_tree(const expression::op_manager& ops,
                            uint32_t top,
                            std::stack<uint32_t>& s) {
    using enum expression::op_type;
    assert(s.empty());
    s.emplace(top);
    while(!s.empty()) {
      uint32_t i = s.top();
      s.pop();
      const expression::op& op = ops.getobj(i);
      switch(op.type) {
        case Or:
          s.emplace(op.bin.l);
          s.emplace(op.bin.r);
          break;
        case Not: {
          const expression::op& v = ops.getobj(op.left());
          assert(v.type == Var);
          o << "-" << v.user_int32 << " ";
          break;
        }
        case Var: {
          o << op.user_int32 << " ";
          break;
        }
        default:
          assert(false);
          break;
      }
    }
    o << "0\n";
  }
};
}
