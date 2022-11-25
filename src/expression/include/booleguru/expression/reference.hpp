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
  // ID 0 is never given to valid references, as the manager base class always
  // counts up from 1 onwards.

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
  inline explicit constexpr reference(const reference& o)
    : mgr_(o.mgr_)
    , id_(o.id_) {}
  inline explicit constexpr reference(reference&& o)
    : mgr_(o.mgr_)
    , id_(o.id_) {}

  inline constexpr bool valid() const { return id_ > 0 && mgr_; }
  inline constexpr uint32_t operator[](uint32_t id) { return get_mgr()[id]; }
  inline constexpr const T* operator->() const {
    return &get_mgr().getobj(id_);
  }
  inline constexpr T* operator->() { return &get_mgr().getobj(id_); }

  inline constexpr T& get_obj() { return *this; }
  inline constexpr const T& get_obj() const { return *this; }

  inline constexpr M& get_mgr() noexcept {
    assert(mgr_);
    return *mgr_;
  }
  inline constexpr const M& get_mgr() const noexcept {
    assert(mgr_);
    return *mgr_;
  }
  inline constexpr void set_mgr(M* m) { mgr_ = m; }
  inline constexpr ref get_id() const noexcept { return id_; }
  inline constexpr void set_id(ref id) noexcept { id_ = id; }

  inline constexpr reference& operator=(const reference& o) {
    mgr_ = o.mgr_;
    id_ = o.id_;
    return *this;
  }
  inline constexpr bool operator==(const reference& o) const {
    return mgr_ == o.mgr_ && id_ == o.id_;
  }
};
}
