#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>

#include <limits>
#include <stack>
#include <unordered_map>
#include <vector>

#include <ankerl/unordered_dense.h>

#include <booleguru/util/die.hpp>

#include "op.hpp"

namespace booleguru::expression {
struct variable;

template<typename R, typename C>
class manager {
  public:
  using ref = typename R::ref;
  using T = typename R::objtype;
  using objref = R;

  protected:
  using map = ankerl::unordered_dense::map<T, ref>;
  using vec = typename map::value_container_type;
  map objects_map_;
  size_t counter_ = 1;

  inline void init() {}

  public:
  manager(size_t size = 2048) {
    objects_map_.reserve(size);
    static_cast<C*>(this)->init();
  };

  inline const vec& objects() const noexcept { return objects_map_.values(); }

  constexpr inline R operator[](ref r) noexcept {
    assert(r < counter_);
    return R(*static_cast<C*>(this), r);
  }
  constexpr inline R operator[](ref r) const noexcept {
    assert(r < counter_);
    const C* child = static_cast<const C*>(this);
    return R(*child, r);
  }
  constexpr inline const T& getobj(ref r) const noexcept {
    assert(r > 0);
    r = r - 1;
    assert(r < counter_);
    return objects()[r].first;
  }

  constexpr inline R get_from_map(const T& obj) noexcept {
    auto it = objects_map_.find(obj);
    if(it == objects_map_.end()) {
      return R();
    }
    return objref(static_cast<C&>(*this), it->second);
  }
  constexpr inline ref get_id_from_map(const T& obj) const noexcept {
    auto it = objects_map_.find(obj);
    if(it == objects_map_.end()) {
      return 0;
    }
    return it->second;
  }

  constexpr inline size_t size() const { return objects_map_.size(); }

  constexpr inline R insert(T&& obj) {
    assert(static_cast<int32_t>(counter_)
           < std::numeric_limits<int32_t>::max());
    ref idx = static_cast<C*>(this)->insert_id(std::move(obj));
    return (*this)[idx];
  }
  constexpr inline ref insert_id(T&& obj) {
    assert(static_cast<int32_t>(counter_)
           < std::numeric_limits<int32_t>::max());
    size_t idx = counter_++;
    if(idx == std::numeric_limits<uint32_t>::max() - 1) {
      util::die("Reached limit of manager !! Have " + std::to_string(counter_)
                + " elements in store. Try GC or something else.");
    }
    objects_map_.insert(std::make_pair(std::move(obj), idx));
    return idx;
  }

  constexpr inline R get(const T& obj) {
    if constexpr(std::is_same<T, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    R ref = static_cast<C*>(this)->get_from_map(obj);
    if(ref.valid())
      return ref;

    return static_cast<C*>(this)->insert(std::move(obj));
  }
  constexpr inline R get(T&& obj) {
    if constexpr(std::is_same<T, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    R ref = static_cast<C*>(this)->get_from_map(obj);
    if(ref.valid())
      return ref;

    return static_cast<C*>(this)->insert(std::move(obj));
  }

  constexpr inline ref get_id(const T& obj) {
    if constexpr(std::is_same<T, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    ref r = static_cast<C*>(this)->get_id_from_map(obj);
    if(r)
      return r;

    return static_cast<C*>(this)->insert_id(obj);
  }
  constexpr inline ref get_id(T&& obj) {
    if constexpr(std::is_same<T, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    ref r = static_cast<C*>(this)->get_id_from_map(obj);
    if(r)
      return r;

    return static_cast<C*>(this)->insert_id(std::move(obj));
  }
};
}
