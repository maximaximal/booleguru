#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>

namespace booleguru::expression {
template<typename T, class M>
class reference {
  public:
  using objtype = T;
  using ref = uint32_t;

  protected:
  M* mgr_;
  ref id_;

  public:
  inline constexpr reference()
    : mgr_(nullptr)
    , id_(0) {}
  inline explicit constexpr reference(M& m, uint32_t id)
    : mgr_(&m)
    , id_(id) {}
  inline explicit constexpr reference(const M& m, uint32_t id)
    : mgr_(const_cast<M*>(&m))
    , id_(id) {}

  inline constexpr operator bool() const { return mgr_; }
  inline constexpr uint32_t operator[](uint32_t id) { return get_mgr()[id]; }
  inline constexpr const T* operator->() const {
    return &get_mgr().getobj(id_);
  }
  inline constexpr T* operator->() { return &get_mgr().getobj(id_); }

  inline constexpr M& get_mgr() {
    assert(mgr_);
    return *mgr_;
  }
  inline constexpr const M& get_mgr() const {
    assert(mgr_);
    return *mgr_;
  }
  inline constexpr ref get_id() const { return id_; }
};
}
