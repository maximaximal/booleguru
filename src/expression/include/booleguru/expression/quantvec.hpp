#pragma once

#include <vector>

#include "op.hpp"

namespace booleguru::expression {
/** Toolkit for manipulating quantifier trees that have been collapsed into a
 * prefix.
 */
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
    explicit entry() noexcept
      : var(0)
      , nesting(0)
      , type(op_type::None)
      , subtree_leaf(false) {}
  };

  /** While a vector is in theory a bad datastructure for this kind of
   * restructuring, the adavntage is that indices stay constant and erases
   * should not cost too much. We keep the implementation of a quantvec fully
   * private though, so if this becomes an issue at some point for huge
   * prefixes, we could swap it against a linked list. The main issue is
   * extracting the critical path and destructively searching, which is n^2 in
   * the worst case. */
  using qvec_t = std::vector<entry>;
  qvec_t v;

  int32_t deepest_quantifier_nesting = 0;

  /** The end of a critical path is the end of the deepest sub-tree. Multiple
   * trees may branch off this tree and must be filtered out when looking at the
   * critical path. */
  int32_t critical_path_end = 0;

  int flip_ctx_count = 0;

  [[nodiscard]] inline bool should_flip() const noexcept {
    return flip_ctx_count % 2 == 1;
  }

  void close_flip_ctx() {
    assert(flip_ctx_count > 0);
    --flip_ctx_count;
  }

  public:
  struct flip_ctx {
    inline constexpr explicit flip_ctx(quantvec& v) noexcept
      : v_(v) {}
    inline constexpr ~flip_ctx() noexcept { v_.close_flip_ctx(); }

    inline constexpr flip_ctx(flip_ctx&) = delete;
    inline constexpr flip_ctx(flip_ctx&&) = delete;

    private:
    quantvec& v_;
  };

  flip_ctx open_flip_ctx() {
    ++flip_ctx_count;
    return flip_ctx(*this);
  }

  [[nodiscard]] static bool consteval inline is_exists(
    const entry& e) noexcept {
    return e.is_exists();
  }
  [[nodiscard]] static bool consteval inline is_forall(
    const entry& e) noexcept {
    return e.is_forall();
  }

  constexpr quantvec(size_t reserve = 256) { v.reserve(reserve); }

  void mark_leaves();

  quantvec extract_critical_path(bool keep = false);

  [[nodiscard]] constexpr size_t size() const noexcept { return v.size(); }
  size_t add(op_type quant_type, uint32_t var, int32_t nesting);

  [[nodiscard]] constexpr bool is_leaf(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].subtree_leaf;
  }
  [[nodiscard]] constexpr int32_t nesting(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].nesting;
  }
  [[nodiscard]] constexpr int32_t var(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].var;
  }

  [[nodiscard]] constexpr op_type type(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].type;
  }
};
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec& q);
