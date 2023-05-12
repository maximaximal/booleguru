#pragma once

#include <vector>

#include "op.hpp"

namespace booleguru::expression {
/** Toolkit for manipulating quantifier trees that have been collapsed into a
 * prefix.
 */
class quantvec {
  public:
  enum class quant_type : uint8_t { Exists = 0, Forall = 1 };

  static inline constexpr quant_type op_type_to_quant_type(op_type op) {
    assert(op == op_type::Forall || op == op_type::Exists);
    return static_cast<quant_type>(static_cast<unsigned int>(op) - 1);
  }
  static inline constexpr op_type quant_type_to_op_type(quant_type quant) {
    return static_cast<op_type>(static_cast<unsigned int>(quant) + 1);
  }

  struct entry {
    uint32_t var;
    int32_t tree_depth : 29;
    quant_type quant : 1;
    bool subtree_leaf : 1 = false;
    bool marked : 1 = false;

    [[nodiscard]] bool consteval inline is_exists() const noexcept {
      return quant == quant_type::Exists;
    }
    [[nodiscard]] bool consteval inline is_forall() const noexcept {
      return quant == quant_type::Forall;
    }

    inline void constexpr mark() noexcept { marked = true; }

    explicit entry(op_type op_type, uint32_t var, int32_t tree_depth) noexcept
      : var(var)
      , tree_depth(tree_depth)
      , quant(op_type_to_quant_type(op_type)) {}
    explicit entry() noexcept
      : var(0)
      , tree_depth(0)
      , quant(quant_type::Forall)
      , subtree_leaf(false) {}
  };

  private:
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

  [[nodiscard]] inline constexpr size_t size() const noexcept {
    return v.size();
  }
  size_t add(op_type quant_type, uint32_t var, int32_t tree_depth);
  inline constexpr void add(const entry& e) { v.emplace_back(e); }

  [[nodiscard]] inline constexpr bool is_leaf(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].subtree_leaf;
  }
  [[nodiscard]] inline constexpr int32_t tree_depth(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].tree_depth;
  }
  [[nodiscard]] inline constexpr int32_t var(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].var;
  }
  [[nodiscard]] inline constexpr quant_type quant(size_t i) const noexcept {
    assert(i < v.size());
    return v[i].quant;
  }
  [[nodiscard]] inline constexpr op_type type(size_t i) const noexcept {
    assert(i < v.size());
    op_type op = quant_type_to_op_type(v[i].quant);
    assert(op == op_type::Forall || op == op_type::Exists);
    return op;
  }
  [[nodiscard]] inline constexpr entry& operator[](size_t i) noexcept {
    assert(i < v.size());
    return v[i];
  }
  [[nodiscard]] inline constexpr const entry& operator[](
    size_t i) const noexcept {
    return const_cast<quantvec&>(*this)[i];
  }

  struct EupAup;
  struct EdownAdown;

  /** @brief Combine two quantvecs according to their nestings.
   *
   */
  template<typename Merger>
  static quantvec merge(quantvec& tgt, quantvec& src);
};
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec::entry& e);

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quantvec& q);
