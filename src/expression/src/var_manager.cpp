#include <ostream>

#include <booleguru/expression/var_manager.hpp>

namespace booleguru::expression {
std::ostream&
operator<<(std::ostream& o, const var_ref& v) {
  return o << v->name;
}

std::ostream&
operator<<(std::ostream& o, const variable& v) {
  return o << "variable{" << v.name << "}";
}
}
