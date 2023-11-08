#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>

#include <limits>
#include <stack>
#include <unordered_map>
#include <vector>

#include <ankerl/unordered_dense.h>

#include <booleguru/expression/op.hpp>
#include <booleguru/util/die.hpp>

namespace booleguru::expression {
struct variable;

template<typename R, typename C>
class manager {
  public:
  using objtype = typename R::objtype;
  using ref = R;
  using id = typename R::id;

  protected:
  using map = ankerl::unordered_dense::map<objtype, id>;
  using vec = typename map::value_container_type;
  map objects_map_;
  size_t counter_ = 1;

  inline void init() {}

  public:
  manager(size_t size = 2048) {
    objects_map_.reserve(size);
    static_cast<C*>(this)->init();
  };

  void reset() noexcept {
    objects_map_.clear();
    counter_ = 1;
    static_cast<C*>(this)->init();
  }

  inline const vec& objects() const noexcept {
    return objects_map_.values();
  }

  constexpr inline R operator[](id r) noexcept {
    assert(r < counter_);
    return R(*static_cast<C*>(this), r);
  }
  constexpr inline R operator[](id r) const noexcept {
    assert(r < counter_);
    const C* child = static_cast<const C*>(this);
    return R(*child, r);
  }
  constexpr inline const objtype& getobj(id r) const noexcept {
    assert(r > 0);
    r = r - 1;
    assert(r < counter_);
    return objects()[static_cast<size_t>(r)].first;
  }

  constexpr inline R get_from_map(const objtype& obj) noexcept {
    auto it = objects_map_.find(obj);
    if(it == objects_map_.end()) {
      return R();
    }
    return R(static_cast<C&>(*this), it->second);
  }
  constexpr inline id get_id_from_map(const objtype& obj) const noexcept {
    auto it = objects_map_.find(obj);
    if(it == objects_map_.end()) {
      return 0;
    }
    return it->second;
  }

  constexpr inline size_t size() const {
    return objects_map_.size();
  }

  constexpr inline R insert(objtype&& obj) {
    assert(static_cast<int32_t>(counter_)
           < std::numeric_limits<int32_t>::max());
    id idx = static_cast<C*>(this)->insert_id(std::move(obj));
    return (*this)[idx];
  }
  constexpr inline id insert_id(objtype&& obj) {
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

  constexpr inline R get(const objtype& obj) {
    if constexpr(std::is_same<objtype, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    R id = static_cast<C*>(this)->get_from_map(obj);
    if(id.valid())
      return id;

    return static_cast<C*>(this)->insert(std::move(obj));
  }

  constexpr inline R get(objtype&& obj) {
    R id = static_cast<C*>(this)->get_from_map(obj);
    if(id.valid())
      return id;

    return static_cast<C*>(this)->insert(std::move(obj));
  }

  constexpr inline id get_id(const objtype& obj) {
    if constexpr(std::is_same<objtype, variable>()) {
      obj = static_cast<C*>(this)->transform_(obj);
    }
    id r = static_cast<C*>(this)->get_id_from_map(obj);
    if(r)
      return r;

    return static_cast<C*>(this)->insert_id(obj);
  }

  constexpr inline id get_id(objtype&& obj) {
    id r = static_cast<C*>(this)->get_id_from_map(obj);
    if(r)
      return r;

    return static_cast<C*>(this)->insert_id(std::move(obj));
  }
};
}
