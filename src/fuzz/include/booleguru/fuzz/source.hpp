#pragma once

#include <cstddef>
#include <cstdint>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::fuzz {
/// Applies a fuzzer input to an op_manager
struct source {
  expression::op_manager& ops;

  source(expression::op_manager& ops)
    : ops(ops) {}

  inline bool op_satisfies_invariants(const expression::op& o) const {
    if(o.type == expression::op_type::None || o.type > expression::op_type::Var) {
      return false;
    }
    if(o.is_quant() || o.is_binop()) {
      if(o.left().id_ == 0 || o.left().id_ > ops.size()) {
        return false;
      }
      if(o.right().id_ == 0 || o.right().id_ > ops.size()) {
        return false;
      }
    }
    if(o.type == expression::op_type::Var) {
      // Don't care about the var manager, variables are just IDs.
      return true;
    }
    if(o.is_unop()) {
      if(o.left().id_ == 0 || o.left().id_ > ops.size()) {
        return false;
      }
    }
    return true;
  }

  void ensure_var_exists(expression::op& o);

  size_t apply(const uint8_t* data_ptr, size_t size);

  void apply_from_file(std::string_view path);
};
}
