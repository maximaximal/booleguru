#pragma once

#include <cstddef>
#include <cstdint>

#include <unordered_map>
#include <vector>

#include "op.hpp"

namespace booleguru::expression {
template<typename R, typename C>
class manager {
  public:
  using ref = typename R::ref;
  using T = typename R::objtype;
  using objref = R;

  protected:
  std::vector<T> objects_;
  std::unordered_map<size_t, ref> objects_map_;

  public:
  manager(size_t size = 2048) {
    objects_.reserve(size);
    objects_map_.reserve(size);

    // Id 0 breaks a lot of stuff, so don't allow id=0.
    objects_.emplace_back();
  };

  constexpr inline R operator[](ref r) { return R(*static_cast<C*>(this), r); }
  constexpr inline R operator[](ref r) const {
    const C* child = static_cast<const C*>(this);
    return R(*child, r);
  }
  constexpr inline T& getobj(ref r) { return objects_[r]; }
  constexpr inline const T& getobj(ref r) const { return objects_[r]; }

  constexpr inline R get_from_map(const T& obj, size_t hash) {
    auto r = objects_map_.equal_range(hash);

    for(auto it = r.first; it != r.second; ++it) {
      auto idx = it->second;
      auto& matched_obj = objects_[idx];

      if(matched_obj == obj) {
        return objref(static_cast<C&>(*this), idx);
      }
    }
    return R();
  }

  constexpr inline R insert(T&& obj, size_t obj_hash) {
    size_t idx = objects_.size();
    objects_.emplace_back(std::move(obj));
    objects_map_.insert({ obj_hash, idx });
    return (*this)[idx];
  }

  constexpr inline R get(const T& obj) {
    auto obj_hash = obj.hash();
    R ref = static_cast<C*>(this)->get_from_map(obj, obj_hash);
    if(ref.valid())
      return ref;

    return static_cast<C*>(this)->insert(std::move(obj), obj_hash);
  }
  constexpr inline R get(T&& obj) {
    auto obj_hash = obj.hash();
    R ref = static_cast<C*>(this)->get_from_map(obj, obj_hash);
    if(ref.valid())
      return ref;

    return static_cast<C*>(this)->insert(std::move(obj), obj_hash);
  }
};
}
