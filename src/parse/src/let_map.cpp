#include <booleguru/parse/let_map.hpp>

namespace booleguru::parse {
expression::bvop_id
let_map::operator[](std::string id) const {
  for(auto it = lets_.rbegin(); it != lets_.rend(); ++it) {
    const bvop_map& m = *it;
    const auto op_it = m.find(id);
    if(op_it != m.end()) {
      return op_it->second;
    }
  }
  return 0;
}

void
let_map::add(std::string id, expression::bvop_id op) {
  bvop_map& m = lets_.back();
  if(m.contains(id)) {
    throw binding_conflict(id);
  }
  m.emplace(std::make_pair(id, op));
}

void
let_map::push() {
  lets_.emplace_back();
}
void
let_map::pop() {
  lets_.pop_back();
}
}
