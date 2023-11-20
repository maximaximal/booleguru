#include "booleguru/expression/bv.hpp"
#include <booleguru/expression/bvop_manager.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/util/expect.hpp>
#include <booleguru/util/postorder.hpp>

#include <booleguru/util/unsupported.hpp>

namespace booleguru::expression {
using enum bvop_type;
using enum op_type;
using expression::op;

bvop_ref
bvop_ref::left() {
  if(!valid())
    return bvop_ref();
  return get_mgr()[id((*this)->left())];
}
bvop_ref
bvop_ref::right() {
  if(!valid())
    return bvop_ref();
  return get_mgr()[id((*this)->right())];
}

// All bit-blasting operations are defined below as individual functions. The
// widths are given in the width_stack parameter, while the OP-structure is
// given in the op_vec parameter. The modifications of the op stack are given as
// BV[n] parameters, where n gives the width of the bit-vector. Each operation
// modifies the op_vec vector according to its specification. At the end, only a
// BV[1] must remain, otherwise the SMT-LIB2 input is malformed.

// In: BV[n]
// Out: BV[n]
static void
encode_bvnot(op_manager& ops,
             bvop_manager& bvops,
             const bvop& bb,
             std::vector<op_id>& op_vec,
             std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  EXPECTE(!width_stack.empty(), tree_traversal);
  uint16_t width = width_stack.top();

  // Width stays constant. Ops are modified in-place to be their notted
  // variants.

  for(uint16_t i = 0; i < width; ++i) {
    size_t j = op_vec.size() - width + i;
    op_vec[j] = ops.get_id(op(op_type::Not, op_vec[j]));
  }
}

// In: BV[1] x BV[1]
// Out: BV[1]
template<op_type t>
static void
encode_bin(op_manager& ops,
           bvop_manager& bvops,
           const bvop& bb,
           std::vector<op_id>& op_vec,
           std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  EXPECTE(!width_stack.empty(), tree_traversal);
  uint16_t width_r = width_stack.top();
  width_stack.pop();
  EXPECTE(!width_stack.empty(), tree_traversal);
  uint16_t width_l = width_stack.top();
  width_stack.pop();

  width_stack.push(1);

  EXPECT(width_l == width_r);
  EXPECT(width_l == 1);

  op_vec[op_vec.size() - 2]
    = ops.get_id(op(t, op_vec[op_vec.size() - 2], op_vec[op_vec.size() - 1]));

  op_vec.pop_back();
}

// In: BV[n] x BV[n]
// Out: BV[n]
template<op_type t>
static void
encode_bvbin(op_manager& ops,
             bvop_manager& bvops,
             const bvop& bb,
             std::vector<op_id>& op_vec,
             std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  assert(!width_stack.empty());
  uint16_t width_r = width_stack.top();
  width_stack.pop();
  assert(!width_stack.empty());
  uint16_t width_l = width_stack.top();
  width_stack.pop();

  width_stack.push(width_l);

  assert(width_l == width_r);
  assert(width_l > 0);

  for(uint16_t i = 0; i < width_l; ++i) {
    size_t j = op_vec.size() - width_l + i;
    size_t jj = op_vec.size() - width_l - width_r + i;
    op_vec[jj] = ops.get_id(op(t, op_vec[jj], op_vec[j]));
  }
  op_vec.resize(op_vec.size() - width_l);
}

// In: BV[n] x BV[n]
// Out: BV[1]
static void
encode_bveq(op_manager& ops,
            bvop_manager& bvops,
            const bvop& bb,
            std::vector<op_id>& op_vec,
            std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  uint16_t width_r = width_stack.top();
  assert(!width_stack.empty());
  width_stack.pop();
  uint16_t width_l = width_stack.top();
  assert(!width_stack.empty());
  width_stack.pop();

  width_stack.push(1);

  assert(width_l == width_r);
  assert(width_l > 0);

  for(uint16_t i = 0; i < width_l; ++i) {
    size_t j = op_vec.size() - width_l + i;
    size_t jj = op_vec.size() - width_l - width_r + i;
    op_vec[jj] = ops.get_id(op(Equi, op_vec[jj], op_vec[j]));
  }
  op_id conj = op_vec[op_vec.size() - width_l - width_r];
  for(uint16_t i = 1; i < width_l; ++i) {
    size_t jj = op_vec.size() - width_l - width_r + i;
    conj = ops.get_id(op(And, conj, op_vec[jj]));
  }
  // 1 element must remain - this is where the last result of the conjunction is
  // stored.
  op_vec[op_vec.size() - width_l - width_r] = conj;
  op_vec.resize(op_vec.size() - width_l - width_r + 1);
}

// In: none
// Out: BV[n]
static void
encode_bvvar(op_manager& ops,
             bvop_manager& bvops,
             const bvop& bb,
             std::vector<op_id>& op_vec,
             std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  width_stack.push(bb.varop.width);
  for(uint16_t i = 0; i < bb.varop.width; ++i) {
    op_vec.push_back(ops.get_id(op(Var, bb.varop.v, 0, i)));
  }
}

// In: BV[n] x BV[1]
// Out: BV[1]
template<op_type t>
static void
encode_bvquant(op_manager& ops,
               bvop_manager& bvops,
               const bvop& bb,
               std::vector<op_id>& op_vec,
               std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  uint16_t width_r = width_stack.top();
  width_stack.pop();
  uint16_t width_l = width_stack.top();
  width_stack.pop();

  width_stack.push(1);

  assert(width_l >= 1);
  assert(width_r == 1);

  op_id q = op_vec[op_vec.size() - 1];
  for(ssize_t i = width_l - 1; i >= 0; --i) {
    size_t j = op_vec.size() - width_l - 1 + i;
    q = ops.get_id(op(t, op_vec[j], q));
  }
  op_vec[op_vec.size() - width_l - 1] = q;
  op_vec.resize(op_vec.size() - width_l);
}

// In: none
// Out: BV[n]
static void
encode_bvconst(op_manager& ops,
               bvop_manager& bvops,
               const bvop& bb,
               std::vector<op_id>& op_vec,
               std::stack<uint16_t>& width_stack) {
  (void)bb;
  (void)bvops;
  width_stack.push(bb.constop.width);
  size_t j = op_vec.size();
  op_vec.resize(op_vec.size() + bb.constop.width);
  for(uint16_t i = 0; i < bb.constop.width; ++i) {
    if(bb.constop.lit.n & (1u << i)) {
      op_vec[i + j] = ops.get_id(op(Var, var_manager::LITERAL_TOP, 0, 0));
    } else {
      op_vec[i + j] = ops.get_id(op(Var, var_manager::LITERAL_BOTTOM, 0, 0));
    }
  }
}

op_ref
bvop_ref::export_as_ops(op_manager& ops) {
  // Main idea: Use a postorder traversal and directly convert each operation to
  // expected chains of direct boolean operations.
  //
  // HANDLING VARIABLES: Variables must be converted from SMT with multi-width
  // into purely variables. This is done using the .i attribute of varops, which
  // makes them unique, but uses the same name.

  auto llink = [this](bvop_id o) -> bvop_id {
    if(o == 0)
      return 0;
    assert(o <= get_mgr().size());
    return get_mgr().getobj(o).left();
  };
  auto rlink = [this](bvop_id o) -> bvop_id {
    if(o == 0)
      return 0;
    assert(o <= get_mgr().size());
    return get_mgr().getobj(o).right();
  };

  std::vector<op_id> op_vec;
  std::stack<uint16_t> width_stack;

  auto visit = [this, &ops, &op_vec, &width_stack](bvop_id b) -> void {
    const bvop& bb = get_mgr().getobj(b);

    // If left and right were identical, the tree traversal leaves out one
    // branch. This means the other branch has to be duplicated on the stacks in
    // order for the traversal to have correct states.
    if(bb.is_binop(bb.type) && bb.binop.l == bb.binop.r) {
      width_stack.push(width_stack.top());
      std::copy(op_vec.begin() + op_vec.size() - width_stack.top(),
                op_vec.begin() + op_vec.size(),
                std::back_inserter(op_vec));
    }

    switch(bb.type) {
      case bvnot:
        encode_bvnot(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvop_type::and_:
        encode_bin<And>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvop_type::or_:
        encode_bin<Or>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvand:
        encode_bvbin<And>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvor:
        encode_bvbin<Or>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bveq:
        encode_bveq(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvvar:
        encode_bvvar(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvforall:
        encode_bvquant<Forall>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvexists:
        encode_bvquant<Exists>(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvconst:
        encode_bvconst(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      default:
        throw util::unsupported(fmt::format("Unsupported SMT-LIB2 op: {}",
                                            bvop_type_to_str(bb.type)));
    }
  };

  util::postorder<bvop_id, decltype(llink), decltype(rlink)> p(llink, rlink);
  p(get_id(), visit);

  assert(!width_stack.empty());
  assert(width_stack.top() == 1);
  assert(op_vec.size() == 1);

  return ops[op_vec[0]];
}

void
bvop_manager::render_as_dot(std::ostream& o, bvop_id id) const noexcept {
  o << "digraph bv {\n";

  std::stack<bvop_id> unvisited;
  unvisited.push(id);
  while(!unvisited.empty()) {
    const bvop_ref& op = (*this)[unvisited.top()];
    unvisited.pop();
    if(op->is_binop(op->type)) {
      o << "  " << op.get_id().id_ << " [label = \""
        << bvop_type_to_str(op->type) << " {" << op.get_id().id_ << "}\"];\n";
      o << "  " << op.get_id().id_ << " -> " << op->left().id_
        << " [ label=\"l\" ];\n";
      o << "  " << op.get_id().id_ << " -> " << op->right().id_
        << " [ label=\"r\" ];\n";
      unvisited.push(op->left());
      unvisited.push(op->right());
    } else if(op->type == bvvar) {
      o << "  " << op.get_id().id_ << " [ label=\"" << op->varop.v.id_ << ","
        << op->varop.width << "\" ];\n";
    } else if(op->type == bvconst) {
      o << "  " << op.get_id().id_ << " [ label=\"" << op->constop.lit.n << ":"
        << op->constop.width << "\" ];\n";
    }
  }
  o << "}" << std::endl;
}

const char*
bvop_type_to_str(bvop_type t) noexcept {
  switch(t) {
    case and_:
      return "and";
    case or_:
      return "or";
    case not_:
      return "not";
    case bvvar:
      return "bvvar";
    case bvconst:
      return "bvconst";
    case bvnot:
      return "bvnot";
    case bvneg:
      return "bvneg";
    case bvand:
      return "bvand";
    case bvor:
      return "bvor";
    case bvadd:
      return "bvadd";
    case bvmul:
      return "bvmul";
    case bvudiv:
      return "bvudiv";
    case bvurem:
      return "bvurem";
    case bvshl:
      return "bvshl";
    case bvlshr:
      return "bvshr";
    case bvult:
      return "bvult";
    case bveq:
      return "bveq";
    case bvforall:
      return "forall";
    case bvexists:
      return "exists";
    case concat:
      return "concat";
    case extract:
      return "extract";
  }
}
}
