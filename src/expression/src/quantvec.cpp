#include <booleguru/expression/quantvec.hpp>

namespace booleguru::expression {
void
quantvec::mark_leaves() {
  size_t last_it = v.size() - 1;
  for(auto it = list.begin(); it != l.end(); ++it) {
    if(last_it != l.end()) {
      last_it->subtree_leaf = last_it->nesting >= it->nesting;

      // Same element on the leaf must also be a leaf. So, once a leaf is
      // found, walk backwards on the same sub-tree until there is some other
      // element with a different op_type, which is when the backwards-walk
      // ends.
      if(last_it->subtree_leaf) {
        auto rit = make_reverse_iterator(last_it);
        ++rit;
        while(rit != l.rend() && rit->t == last_it->t &&
              rit->nesting + 1 == last_it->nesting) {
          rit->subtree_leaf = true;
          ++rit;
        }
      }
    }
    last_it = it;
  }
  // Last entry MUST be a leaf.
  l.rbegin()->subtree_leaf = true;
}
}
