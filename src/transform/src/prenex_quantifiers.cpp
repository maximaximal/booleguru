#include <booleguru/transform/prenex_quantifiers.hpp>

#include <iostream>

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e) {
  char leaf = e.subtree_leaf ? 'L' : 'I';
  return o << e.t << ":" << e.nesting << leaf;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::list<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}
