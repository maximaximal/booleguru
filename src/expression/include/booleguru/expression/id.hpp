// Author: Marcel Simader (marcel.simader@jku.at)
// Date: 11.09.2023

#pragma once

#include <cassert>
#include <cstdint>
#include <limits>

namespace booleguru::expression {

/* An `id` is a shallow wrapper around a `uint32_t`. This only serves for static
 * type checking.
 */
template<typename T>
struct id {
  uint32_t id_;

  explicit constexpr id(uint32_t id_ = 0)
    : id_(id_){};

  inline constexpr T& operator=(T const& o_id) {
    if(this == &o_id)
      return *this;
    id_ = static_cast<uint32_t>(o_id);
    return *this;
  }

  inline constexpr T operator+(uint32_t o_id) const { return T(id_ + o_id); }
  inline constexpr T operator+(id const& o_id) const {
    return T(id_ + o_id.id_);
  }
  inline constexpr T operator-(uint32_t o_id) const { return T(id_ - o_id); }
  inline constexpr T operator-(id const& o_id) const {
    return T(id_ - o_id.id_);
  }
  inline constexpr bool operator>(uint32_t o_id) const { return id_ > o_id; }
  inline constexpr bool operator>(id const& o_id) const {
    return id_ > o_id.id_;
  }
  inline constexpr bool operator<(uint32_t o_id) const { return id_ < o_id; }
  inline constexpr bool operator<(id const& o_id) const {
    return id_ < o_id.id_;
  }
  inline constexpr bool operator==(uint32_t o_id) const { return id_ == o_id; }
  inline constexpr bool operator==(id const& o_id) const {
    return id_ == o_id.id_;
  }
  inline constexpr void operator++() { ++id_; }

  inline explicit constexpr operator bool() const { return id_ != 0; }
  inline explicit constexpr operator uint16_t() const {
    assert(id_ < std::numeric_limits<uint16_t>::max());
    return id_;
  }
  inline explicit constexpr operator uint32_t() const { return id_; }
  inline explicit constexpr operator std::size_t() const { return id_; }
};

struct var_id : public id<var_id> {
  constexpr var_id(uint32_t id_ = 0)
    : id(id_){};
};
struct op_id : public id<op_id> {
  constexpr op_id(uint32_t id_ = 0)
    : id(id_){};
};
struct bvop_id : public id<bvop_id> {
  constexpr bvop_id(uint32_t id_ = 0)
    : id(id_){};
};
struct script_id : public id<script_id> {
  constexpr script_id(uint32_t id_ = 0)
    : id(id_){};
};
}
