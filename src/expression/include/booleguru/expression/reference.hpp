#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {
template<class T, class M, class ID = id>
class reference {
  public:
  using objtype = T;
  using ref = ID;
  using numeric_type = typename ID::numeric_type;

  protected:
  // ID 0 is never given to valid references, as the manager base class always
  // counts up from 1 onwards.

  M* mgr_;
  ref id_;

  public:
  inline constexpr reference()
    : mgr_(nullptr)
    , id_(0) {}
  inline explicit constexpr reference(M& m, ID id)
    : mgr_(&m)
    , id_(id) {}
  inline explicit constexpr reference(const M& m, ID id)
    : mgr_(const_cast<M*>(&m))
    , id_(id) {}
  inline explicit constexpr reference(const reference& o)
    : mgr_(o.mgr_)
    , id_(o.id_) {}
  inline explicit constexpr reference(reference&& o)
    : mgr_(o.mgr_)
    , id_(o.id_) {}

  inline constexpr bool valid() const { return id_ > 0 && mgr_; }
  inline constexpr ID operator[](ID id) { return get_mgr()[id]; }
  inline constexpr const T* operator->() const {
    return &get_mgr().getobj(id_);
  }
  inline constexpr const T* operator->() { return &get_mgr().getobj(id_); }

  inline constexpr const T& get_obj() const { return get_mgr().getobj(id_); }

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

  inline size_t hash() const {
    size_t hash = reinterpret_cast<intptr_t>(&get_mgr());
    hash <<= 32u;
    hash |= get_id();
    return hash;
  }
};
}
