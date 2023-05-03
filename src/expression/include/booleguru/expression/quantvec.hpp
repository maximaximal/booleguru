#pragma once

#include <vector>

#include "op.hpp"

namespace booleguru::expression {
class quantvec {
  struct entry {
    uint32_t var;
    int32_t nesting : 27;
    op_type type : 4;
    bool subtree_leaf : 1 = false;

    [[nodiscard]] bool consteval inline is_exists() const noexcept {
      return type == op_type::Exists;
    }
    [[nodiscard]] bool consteval inline is_forall() const noexcept {
      return type == op_type::Forall;
    }

    explicit entry(op_type quant_type, uint32_t var, int32_t nesting) noexcept
      : var(var)
      , nesting(nesting)
      , type(quant_type) {}
  };

  using qvec_t = std::vector<entry>;
  qvec_t v;

  public:
  [[nodiscard]] static bool consteval inline is_exists(
    const entry& e) noexcept {
    return e.is_exists();
  }
  [[nodiscard]] static bool consteval inline is_forall(
    const entry& e) noexcept {
    return e.is_forall();
  }

  void mark_leaves();

  [[nodiscard]] constexpr size_t size() const noexcept { return v.size(); }
  void add(op_type quant_type, uint32_t var, int32_t nesting);

  [[nodiscard]] constexpr bool is_leaf(size_t i) const noexcept {
    return v[i].subtree_leaf;
  }
};
}
