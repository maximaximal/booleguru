#include <booleguru/expression/bvop_manager.hpp>
#include <booleguru/expression/op_manager.hpp>

#include <booleguru/util/postorder.hpp>

namespace booleguru::expression {
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
export_as_ops(op_manager& ops) {
  // Main idea: Use a postorder traversal and directly convert each operation to
  // expected chains of direct boolean operations.
  //
  // HANDLING VARIABLES: Variables must be converted from SMT with multi-width
  // into purely variables. This is done using the .i attribute of varops, which
  // makes them unique, but uses the same name.
  (void)ops;
  return op_ref();
}
}
