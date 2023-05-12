#include <algorithm>
#include <iostream>
#include <limits>
#include <ranges>
#include <stack>

#include <booleguru/expression/quanttree.hpp>

using std::cout;
using std::endl;

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree& q) {
  bool first = true;
  for(size_t i = 0; i < q.size(); ++i) {
    if(first)
      first = false;
    else
      o << ", ";

    q[i].stream(o);
  }
  return o;
}

namespace booleguru::expression {
std::ostream&
booleguru::expression::quanttree::entry::stream(std::ostream& o) const {
  if(is_fork()) {
    o << f.left << ":" << f.right;
  } else {
    o << p.type << ":" << p.var;
    if(has_next())
      o << "." << p.next;
  }
  return o;
}

static_assert(sizeof(quanttree::entry) == 16);

uint32_t
quanttree::add(op_type quant_type, uint32_t var, uint32_t next) {
  size_t s = v.size();

  ++number_of_quantifiers;

  op_type t = should_flip() ? op_type_flip_quantifier(quant_type) : quant_type;
  if(v.emplace_back(t, var, next).has_next()) {
    assert(next < v.size());
    v[next].p.parent = s;
  }

  return s;
}

uint32_t
quanttree::add(op_type quant_type, uint32_t var) {
  return add(quant_type, var, std::numeric_limits<uint32_t>::max());
}

uint32_t
quanttree::add(uint32_t left, uint32_t right) {
  size_t s = v.size();
  v.emplace_back(left, right);
  assert(left < v.size());
  assert(right < v.size());
  v[left].f.parent = s;
  v[right].f.parent = s;
  return s;
}

quanttree::quantvec
quanttree::compute_critical_path(uint32_t root) {
  assert(root < v.size());
  quantvec qv;
  struct st {
    uint32_t i;
    uint32_t logical_depth;
    uint32_t syntactical_depth;

    st(uint32_t i, uint32_t l, uint32_t s)
      : i(i)
      , logical_depth(l)
      , syntactical_depth(s) {}
  };
  std::stack<st> s;

  s.emplace(root, 1, 1);

  uint32_t deepest_log = 0;
  uint32_t deepest_syn = 0;
  uint32_t deepest_entry_i = 0;

  while(!s.empty()) {
    uint32_t entry_i = s.top().i;
    uint32_t logical_depth = s.top().logical_depth;
    uint32_t syntactical_depth = s.top().syntactical_depth;
    const entry& entry = v[entry_i];
    s.pop();

    if(entry.is_fork()) {
      s.emplace(entry.f.left, logical_depth, syntactical_depth + 1);
      s.emplace(entry.f.right, logical_depth, syntactical_depth + 1);
    } else if(entry.has_next()) {
      s.emplace(entry.f.right, logical_depth + 1, syntactical_depth + 1);
    } else if(deepest_log < logical_depth) {
      deepest_log = logical_depth;
      deepest_syn = syntactical_depth;
      deepest_entry_i = entry_i;
    }
  }

  assert(deepest_syn > 0);

  qv.resize(deepest_syn);
  for(uint32_t i = deepest_syn; i > 0; --i) {
    qv[i - 1] = deepest_entry_i;
    deepest_entry_i = v[deepest_entry_i].p.parent;
  }

  return qv;
}

std::ostream&
quanttree::to_dot(std::ostream& o) {
  o << "digraph {\n";
  for(size_t i = 0; i < v.size(); ++i) {
    const entry& e = v[i];
    o << i << " [ label=\"";
    e.stream(o) << "\"];\n";

    if(e.is_fork()) {
      o << i << "->" << e.f.left << ";\n";
      o << i << "->" << e.f.right << ";\n";
    } else if(e.has_next()) {
      o << i << "->" << e.p.next << ";\n";
    }
  }
  o << "}\n";
  return o;
}

std::ostream&
quanttree::to_dot(std::ostream& o, quantvec vec) {
  o << "digraph {\n";
  for(size_t vec_i = 0; vec_i < vec.size(); ++vec_i) {
    size_t i = vec[vec_i];
    const entry& e = v[i];
    o << i << " [ label=\"";
    e.stream(o) << "\"];\n";

    if(e.is_fork()) {
      if(std::find(vec.begin(), vec.end(), e.f.left) != vec.end())
        o << i << "->" << e.f.left << ";\n";
      if(std::find(vec.begin(), vec.end(), e.f.right) != vec.end())
        o << i << "->" << e.f.right << ";\n";
    } else if(e.has_next()) {
      if(std::find(vec.begin(), vec.end(), e.p.next) != vec.end())
        o << i << "->" << e.p.next << ";\n";
    }
  }
  o << "}\n";
  return o;
}
}
