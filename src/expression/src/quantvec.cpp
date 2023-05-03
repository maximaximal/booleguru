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

size_t
quantvec::add(op_type quant_type, uint32_t var, int32_t nesting) {
  size_t s = v.size();

  if(deepest_quantifier_nesting < nesting) {
    critical_path_end = s;
    deepest_quantifier_nesting = nesting;
  }

  op_type t = should_flip() ? op_type_flip_quantifier(quant_type) : quant_type;
  v.emplace_back(t, var, nesting);
  return s;
}

quantvec
quantvec::extract_critical_path(bool keep) {
  quantvec c(deepest_quantifier_nesting + 1);
  c.v.resize(deepest_quantifier_nesting + 1);
  for(ssize_t i = critical_path_end, j = deepest_quantifier_nesting; i >= 0;
      --i) {
    if(v[i].nesting == j) {
      c.v[j] = v[i];
      if(!keep) {
        v.erase(v.begin() + i);
      }
      if(j == 0) {
        break;
      } else {
        --j;
      }
    }
  }
  if(!keep) {
    deepest_quantifier_nesting = 0;
    critical_path_end = 0;
  }
  return c;
}
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec& q) {
  bool first = true;
  for(size_t i = 0; i < q.size(); ++i) {
    if(first)
      first = false;
    else
      o << ", ";

    char inner = q.is_leaf(i) ? 'L' : 'I';

    o << q.var(i) << "/" << q.nesting(i) << "/" << q.type(i) << "/" << inner;
  }
  return o;
}
