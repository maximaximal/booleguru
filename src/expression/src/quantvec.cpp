#include <booleguru/expression/quantvec.hpp>

#include <algorithm>
#include <limits>
#include <ranges>

#include <iostream>
using std::cout;
using std::endl;

namespace booleguru::expression {
void
quantvec::mark_leaves() {
  // Only putting this here as quantvec::entry is private. The entry should be
  // very compact, otherwise the std::vector becomes even worse.
  static_assert(sizeof(entry) == 8);
  static_assert(static_cast<unsigned int>(op_type::Forall) - 1 ==
                static_cast<unsigned int>(quant_type::Forall));
  static_assert(static_cast<unsigned int>(op_type::Exists) - 1 ==
                static_cast<unsigned int>(quant_type::Exists));
  static_assert(op_type_to_quant_type(op_type::Forall) == quant_type::Forall);
  static_assert(op_type_to_quant_type(op_type::Exists) == quant_type::Exists);
  static_assert(quant_type_to_op_type(quant_type::Forall) == op_type::Forall);
  static_assert(quant_type_to_op_type(quant_type::Exists) == op_type::Exists);

  size_t last_i = v.size() - 1;
  for(size_t i = 0; i != v.size(); ++i) {
    if(last_i != v.size() - 1) {
      auto& last_i_obj = v[last_i];
      last_i_obj.subtree_leaf = last_i_obj.tree_depth >= v[i].tree_depth;

      // Same element on the leaf must also be a leaf. So, once a leaf is
      // found, walk backwards on the same sub-tree until there is some other
      // element with a different op_type, which is when the backwards-walk
      // ends.
      if(last_i_obj.subtree_leaf) {
        ssize_t ri = last_i;
        while(ri >= 0 && v[ri].quant == last_i_obj.quant &&
              v[ri].tree_depth + 1 == last_i_obj.tree_depth) {
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
    if(v[i].tree_depth == j) {
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

/** @brief Combine two quantvecs according to their nestings.
 *
 */
template<class Merger>
quantvec
quantvec::merge(quantvec& tgt, quantvec& src) {
  quantvec r(tgt.size() + src.size());
  uint32_t tgt_idx = 0;
  uint32_t src_idx = 0;
  while(r.size() != tgt.size() + src.size() && tgt_idx < tgt.size()) {
    // Start with the current reference entry.
    entry& e = tgt[tgt_idx++];

    // Always has to be inserted to the result.
    cout << "Adding Critical: " << e.var << endl;
    r.add(e), e.mark();

    // Now, we search for elements in the src quantvec that can be inserted.
    for(uint32_t i = src_idx; i < src.size(); ++i) {
      cout << "I: " << i << endl;
      entry& subtree_start = src[i];
      if(subtree_start.marked)
        continue;

      auto pred = [&i, &src, &e]() {
        return i < src.size() && src[i].quant == e.quant &&
               src[i].tree_depth >= e.tree_depth;
      };

      if(pred()) {
        while(pred()) {
          entry& s = src[i];
          r.add(s), s.mark(), ++i;
          cout << "Adding : " << s.var << endl;
        }
      } else {
        // Ignore this sub-tree
        break;
      }
    }
  }
  return r;
}

template quantvec
quantvec::merge<quantvec::EupAup>(quantvec& tgt, quantvec& src);
template quantvec
quantvec::merge<quantvec::EdownAdown>(quantvec& tgt, quantvec& src);
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec::entry& e) {
  char inner = e.subtree_leaf ? 'L' : 'I';
  return o << e.var << "/" << e.tree_depth << "/"
           << booleguru::expression::quantvec::quant_type_to_op_type(e.quant)
           << "/" << inner;
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec& q) {
  bool first = true;
  for(size_t i = 0; i < q.size(); ++i) {
    if(first)
      first = false;
    else
      o << ", ";

    o << q[i];
  }
  return o;
}
