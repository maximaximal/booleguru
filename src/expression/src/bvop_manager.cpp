#include <booleguru/expression/bvop_manager.hpp>
#include <booleguru/expression/op_manager.hpp>

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
      case bvnot: {
        uint16_t width = width_stack.top();

        // Width stays constant. Ops are modified in-place to be their notted
        // variants.

        for(uint16_t i = 0; i < width; ++i) {
          size_t j = op_vec.size() - width + i;
          op_vec[j] = ops.get_id(op(op_type::Not, op_vec[j]));
        }
        break;
      }
    case bvop_type::and_: {
        uint16_t width_l = width_stack.top();
        width_stack.pop();
        uint16_t width_r = width_stack.top();
        width_stack.pop();

        width_stack.push(1);

        assert(width_l == width_r);
        assert(width_l == 1);

        op_vec[op_vec.size() - 2] = ops.get_id(
          op(And, op_vec[op_vec.size() - 2], op_vec[op_vec.size() - 1]));

        op_vec.pop_back();
        break;
      }
      case bvand: {
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
        break;
      }
      case bvvar: {
        width_stack.push(bb.varop.width);
        for(uint16_t i = 0; i < bb.varop.width; ++i) {
          op_vec.push_back(ops.get_id(op(Var, bb.varop.v, 0, i)));
        }
        break;
      }
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
