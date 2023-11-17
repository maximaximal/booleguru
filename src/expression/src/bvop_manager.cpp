#include <booleguru/expression/bvop_manager.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

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
static void
encode_and(op_manager& ops,
           bvop_manager& bvops,
           const bvop& bb,
           std::vector<op_id>& op_vec,
           std::stack<uint16_t>& width_stack) {
  uint16_t width_l = width_stack.top();
  width_stack.pop();
  uint16_t width_r = width_stack.top();
  width_stack.pop();

  width_stack.push(1);

  assert(width_l == width_r);
  assert(width_l == 1);

  op_vec[op_vec.size() - 2]
    = ops.get_id(op(And, op_vec[op_vec.size() - 2], op_vec[op_vec.size() - 1]));

  op_vec.pop_back();
}

// In: BV[n] x BV[n]
// Out: BV[n]
static void
encode_bvand(op_manager& ops,
             bvop_manager& bvops,
             const bvop& bb,
             std::vector<op_id>& op_vec,
             std::stack<uint16_t>& width_stack) {
  uint16_t width_l = width_stack.top();
  width_stack.pop();
  uint16_t width_r = width_stack.top();
  width_stack.pop();

  width_stack.push(width_l);

  assert(width_l == width_r);
  assert(width_l > 0);

  for(uint16_t i = 0; i < width_l; ++i) {
    size_t j = op_vec.size() - width_l + i;
    size_t jj = op_vec.size() - width_l - width_r + i;
    op_vec[jj] = ops.get_id(op(And, op_vec[jj], op_vec[j]));
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
  uint16_t width_l = width_stack.top();
  width_stack.pop();
  uint16_t width_r = width_stack.top();
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
  width_stack.push(bb.varop.width);
  for(uint16_t i = 0; i < bb.varop.width; ++i) {
    op_vec.push_back(ops.get_id(op(Var, bb.varop.v, 0, i)));
  }
}

// In: none
// Out: BV[n]
static void
encode_bvconst(op_manager& ops,
               bvop_manager& bvops,
               const bvop& bb,
               std::vector<op_id>& op_vec,
               std::stack<uint16_t>& width_stack) {
  width_stack.push(bb.constop.width);
  size_t j = op_vec.size();
  op_vec.resize(op_vec.size() + bb.constop.width);
  for(uint16_t i = 0; i < bb.constop.width; ++i) {
    if(bb.constop.lit.n & (1u << i)) {
      op_vec[i + j] = ops.get_id(op(Var, var_manager::LITERAL_TOP, 0, i));
    } else {
      op_vec[i + j] = ops.get_id(op(Var, var_manager::LITERAL_BOTTOM, 0, i));
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

    switch(bb.type) {
      case bvnot:
        encode_bvnot(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvop_type::and_:
        encode_and(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvand:
        encode_bvand(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bveq:
        encode_bveq(ops, get_mgr(), bb, op_vec, width_stack);
        break;
      case bvvar:
        encode_bvvar(ops, get_mgr(), bb, op_vec, width_stack);
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

  assert(width_stack.top() == 1);
  assert(op_vec.size() == 1);

  return ops[op_vec[0]];
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
    case concat:
      return "concat";
    case extract:
      return "extract";
  }
}
}
