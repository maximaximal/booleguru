#include <booleguru/transform/prenex_quantifiers.hpp>

#include <iostream>

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e) {
  return o << e.t << ":" << e.nesting;
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
