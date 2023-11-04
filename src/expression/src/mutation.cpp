#include <iostream>

#include <booleguru/expression/mutation.hpp>

namespace booleguru::expression {
std::ostream&
operator<<(std::ostream& o, const mutation& m) {
  using enum mutation;
  switch(m) {
    case push:
      o << "push";
      break;
    case pop:
      o << "pop";
      break;
    case change:
      o << "change";
      break;
  }
  return o;
}
}
