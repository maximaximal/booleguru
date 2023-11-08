#include <booleguru/fuzz/source.hpp>

#include <booleguru/expression/var_manager.hpp>

namespace booleguru::fuzz {
void
source::ensure_var_exists(expression::op& o) {
  if(o.type == expression::op_type::Var) {
    // Replace variable ID with a valid one.
    uint32_t id = o.var.v.id_;
    std::string v_name = std::to_string(id);
    uint32_t new_id
      = ops.vars().get_id(std::move(expression::variable{ v_name })).id_;
    o.var.v = new_id;
  }
}

size_t
source::apply(const uint8_t* data_ptr, size_t size) {
  size_t inserted = 0;
  size_t i = 0;
  for(size_t i = 0; (i + 1) * sizeof(expression::op) < size; ++i) {
    const expression::op* o = reinterpret_cast<const expression::op*>(
      data_ptr + (i * sizeof(expression::op)));
    expression::op o_ = *o;

    if(!op_satisfies_invariants(o_)) {
      continue;
    }

    ensure_var_exists(o_);

    ops.get(std::move(o_));
    ++inserted;
  }

  return inserted;
}
}
