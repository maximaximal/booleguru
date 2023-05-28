#include <algorithm>
#include <fstream>
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

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::entry& e) {
  return e.stream(o);
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::path& p) {
  o << p.type << ":" << p.var;
  if(p.has_next())
    o << "." << p.next;
  return o;
}

namespace booleguru::expression {
std::ostream&
booleguru::expression::quanttree::entry::stream(std::ostream& o) const {
  if(is_fork()) {
    o << f.left << ":" << f.right;
  } else {
    o << p.type << ":" << p.var;
  }
  return o;
}

uint32_t
quanttree::add(op_type quant_type, uint32_t var, uint32_t next) {
  size_t s = v.size();

  ++number_of_quantifiers;

  op_type t = should_flip() ? op_type_flip_quantifier(quant_type) : quant_type;
  if(v.emplace_back(t, var, next).has_next()) {
    assert(next < v.size());
    v[next].parent_ = s;
  }

  return s;
}

uint32_t
quanttree::add(op_type quant_type, uint32_t var) {
  return add(quant_type, var, std::numeric_limits<uint32_t>::max());
}

void
quanttree::splice_path_after_path(uint32_t path, uint32_t insert) {
  assert(v[path].is_path());
  assert(v[insert].is_path());
  assert(v[v[insert].parent_].is_fork());

  remove_entry(insert);

  mark_critical_path(insert);

  uint32_t last = last_entry_on_critical_path(insert);

  v[last].p.next = v[path].p.next;
  v[v[path].p.next].parent_ = last;
  v[insert].parent_ = path;
  v[path].p.next = insert;
}

uint32_t
quanttree::splice_path_before_path(uint32_t path, uint32_t insert) {
  assert(v[path].is_path());
  assert(v[insert].is_path());
  assert(v[v[insert].parent_].is_fork());

  remove_entry(insert);

  mark_critical_path(insert);

  uint32_t last = last_entry_on_critical_path(insert);

  v[last].p.next = v[path].p.next;
  if(v[path].has_next())
    v[v[path].p.next].parent_ = last;
  v[insert].parent_ = path;
  v[path].p.next = insert;

  return last;
}

void
quanttree::remove_entry(uint32_t entry, uint32_t next) {
  quanttree::entry& e = v[entry];
  quanttree::entry& parent = v[e.parent_];
  quanttree::entry& n = v[next];
  assert(e.has_parent());

  if(parent.is_fork()) {
    if(parent.f.left == entry) {
      parent.f.left = next;
    }
    if(parent.f.right == entry) {
      parent.f.right = next;
    }
    n.parent_ = e.parent_;
  } else {
    parent.p.next = next;
    n.parent_ = e.parent_;
  }
}

void
quanttree::remove_entry(uint32_t entry) {
  quanttree::entry& e = v[entry];
  quanttree::entry& parent = v[e.parent_];
  assert(e.has_parent());

  if(parent.is_fork()) {
    parent.is_fork_ = false;
    if(parent.f.left == entry) {
      remove_entry(e.parent_, parent.f.right);
    }
    if(parent.f.right == entry) {
      remove_entry(e.parent_, parent.f.left);
    }
  } else {
    parent.void_next();
  }
}

uint32_t
quanttree::marked_contains_forks(uint32_t i) {
  assert(!v[0].is_fork());

  while(i < v.size()) {
    entry& e = v[i];
    if(e.is_fork_)
      return i;
    i = e.p.next;
  }
  return 0;
}

uint32_t
quanttree::add(uint32_t left, uint32_t right) {
  size_t s = v.size();
  v.emplace_back(left, right);
  assert(left < v.size());
  assert(right < v.size());
  v[left].parent_ = s;
  v[right].parent_ = s;
  return s;
}

void
quanttree::mark_critical_path(uint32_t root) {
  assert(root < v.size());
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

  for(uint32_t i = deepest_syn; i > 0; --i) {
    v[deepest_entry_i].mark();
    deepest_entry_i = v[deepest_entry_i].parent_;
  }
}

std::ostream&
quanttree::to_dot(std::string_view name, std::ostream& o, uint32_t root) {
  std::stack<uint32_t> unvisited;
  unvisited.emplace(root);

  o << "digraph " << name << " {\n";
  while(!unvisited.empty()) {
    uint32_t i = unvisited.top();
    unvisited.pop();
    const entry& e = v[i];
    o << i << " [ label=\"";
    e.stream(o) << "\"";
    if(e.marked_) {
      o << ", color=\"red\"";
    }
    o << "];\n";

    if(e.is_fork()) {
      o << i << "->" << e.f.left << ";\n";
      o << i << "->" << e.f.right << ";\n";
      unvisited.emplace(e.f.left);
      unvisited.emplace(e.f.right);
    } else if(e.has_next()) {
      o << i << "->" << e.p.next << ";\n";
      unvisited.emplace(e.p.next);
    }
  }
  o << "}\n";
  return o;
}

bool
quanttree::should_inline_EupAup(direction dir,
                                const quanttree::entry& pos,
                                const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return false;
  return false;
}
bool
quanttree::should_inline_EdownAdown(direction dir,
                                    const quanttree::entry& pos,
                                    const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return false;
  if(dir == direction::upwards)
    return pos.p.type == possible_inline.p.type;
  return false;
}
bool
quanttree::should_inline_EupAdown(direction dir,
                                  const quanttree::entry& pos,
                                  const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == op_type::Exists && pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return pos.p.type == op_type::Forall && pos.p.type == possible_inline.p.type;
  return false;
}
bool
quanttree::should_inline_EdownAup(direction dir,
                                  const quanttree::entry& pos,
                                  const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == op_type::Forall && pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return pos.p.type == op_type::Exists && pos.p.type == possible_inline.p.type;
  return false;
}

void
quanttree::activate_animation(const std::string& path) {
  animation_path = path;
  animate = true;
}

void
quanttree::create_animation_step(uint32_t root) {
  std::string name = "step" + std::to_string(animate_step);
  ++animate_step;
  std::ofstream f(animation_path + "_" + name + ".dot");
  to_dot(name, f, root);
}
}
