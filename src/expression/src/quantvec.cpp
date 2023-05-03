#include <booleguru/expression/quantvec.hpp>

namespace booleguru::expression {
void
quantvec::mark_leaves() {
  size_t last_i = v.size() - 1;
  for(size_t i = 0; i != v.size(); ++i) {
    if(last_i != v.size() - 1) {
      auto& last_i_obj = v[last_i];
      last_i_obj.subtree_leaf = last_i_obj.nesting >= v[i].nesting;

      // Same element on the leaf must also be a leaf. So, once a leaf is
      // found, walk backwards on the same sub-tree until there is some other
      // element with a different op_type, which is when the backwards-walk
      // ends.
      if(last_i_obj.subtree_leaf) {
        ssize_t ri = last_i;
        while(ri >= 0 && v[ri].type == last_i_obj.type &&
              v[ri].nesting + 1 == last_i_obj.nesting) {
          v[ri].subtree_leaf = true;
          --ri;
        }
      }
    }
    last_i = i;
  }
  // Last entry MUST be a leaf.
  v[v.size() - 1].subtree_leaf = true;
}

void
quantvec::add(op_type quant_type, uint32_t var, int32_t nesting) {
  v.emplace_back(quant_type, var, nesting);
}
}
