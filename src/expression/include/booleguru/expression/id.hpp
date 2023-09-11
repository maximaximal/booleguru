// Author: Marcel Simader (marcel.simader@jku.at)
// Date: 11.09.2023
// (c) Marcel Simader 2023, Johannes Kepler Universität Linz

#pragma once

#include <cassert>
#include <cstdint>

namespace booleguru::expression {
struct id {
  using numeric_type = uint32_t;
  numeric_type id_;

  constexpr id(numeric_type id_ = 0)
    : id_(id_){};

  inline constexpr id& operator=(id const& o_id) {
    if(this == &o_id)
      return *this;
    id_ = (numeric_type)o_id;
    return *this;
  }

  inline constexpr operator numeric_type() const { return id_; }
};

/* A `var_id` is a shallow wrapper around a `uint32_t`. This only serves for
 * static type checking.
 */
struct var_id : public id {
  constexpr var_id(numeric_type id_ = 0)
    : id(id_){};
};

/* An `op_id` is a shallow wrapper around a `uint32_t`. This only serves for
 * static type checking.
 */
struct op_id : public id {
  constexpr op_id(numeric_type id_ = 0)
    : id(id_){};
};
}

