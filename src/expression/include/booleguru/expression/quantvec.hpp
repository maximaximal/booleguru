#pragma once

#include <vector>

#include "op.hpp"

namespace booleguru::expression {
class quantvec {
  struct entry {
    uint32_t var;
    int32_t nesting : 27;
    op_type t : 4;
    bool subtree_leaf : 1 = false;

    [[nodiscard]] bool consteval inline is_exists() const noexcept {
      return t == op_type::Exists;
    }
    [[nodiscard]] bool consteval inline is_forall() const noexcept {
      return t == op_type::Forall;
    }
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
};
}
