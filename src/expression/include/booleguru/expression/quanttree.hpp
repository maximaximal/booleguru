#pragma once

#include <ostream>
#include <vector>

#include "op.hpp"

namespace booleguru::expression {
/** Toolkit for manipulating quantifier trees */
class quanttree {
  public:
  struct path {
    uint32_t var;
    uint32_t next;
    uint32_t parent = std::numeric_limits<uint32_t>::max();
    bool is_fork = false;
    bool marked = false;
    op_type type;

    explicit path(op_type op, uint32_t var, uint32_t next) noexcept
      : var(var)
      , next(next)
      , type(op) {}
  };
  struct fork {
    uint32_t left;
    uint32_t right;
    uint32_t parent = std::numeric_limits<uint32_t>::max();
    bool is_fork = true;
    bool marked = false;
    op_type type;

    explicit fork(uint32_t left, uint32_t right) noexcept
      : left(left)
      , right(right)
      , is_fork(true) {}
  };

  union entry {
    path p;
    fork f;

    [[nodiscard]] constexpr inline bool is_fork() const noexcept {
      return p.is_fork;
    }

    [[nodiscard]] bool constexpr inline is_exists() const noexcept {
      assert(!is_fork());
      return p.type == op_type::Exists;
    }
    [[nodiscard]] bool constexpr inline is_forall() const noexcept {
      assert(!is_fork());
      return p.type == op_type::Forall;
    }
    [[nodiscard]] bool constexpr inline has_next() const noexcept {
      assert(!is_fork());
      return p.next != std::numeric_limits<uint32_t>::max();
    }
    [[nodiscard]] bool constexpr inline has_parent() const noexcept {
      assert(!is_fork());
      return p.next != std::numeric_limits<uint32_t>::max();
    }

    inline void constexpr mark() noexcept { p.marked = true; }

    explicit entry(op_type op_type, uint32_t var, uint32_t next) noexcept
      : p(op_type, var, next) {}
    explicit entry(uint32_t left, uint32_t right) noexcept
      : f(left, right) {}

    std::ostream& stream(std::ostream&) const;
  };

  private:
  using qvec_t = std::vector<entry>;
  qvec_t v;

  uint32_t number_of_quantifiers = 0;
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
    inline constexpr explicit flip_ctx(quanttree& v) noexcept
      : v_(v) {}
    inline constexpr ~flip_ctx() noexcept { v_.close_flip_ctx(); }

    inline constexpr flip_ctx(flip_ctx&) = delete;
    inline constexpr flip_ctx(flip_ctx&&) = delete;

    private:
    quanttree& v_;
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

  constexpr quanttree(size_t reserve = 256) { v.reserve(reserve); }

  [[nodiscard]] inline constexpr size_t size() const noexcept {
    return v.size();
  }
  [[nodiscard]] inline constexpr entry& operator[](uint32_t i) noexcept {
    return v[i];
  }
  [[nodiscard]] inline constexpr const entry& operator[](
    uint32_t i) const noexcept {
    return const_cast<quanttree&>(*this)[i];
  }

  uint32_t add(op_type quant_type, uint32_t var, uint32_t next);
  uint32_t add(op_type quant_type, uint32_t var);
  uint32_t add(uint32_t left, uint32_t right);

  using quantvec = std::vector<uint32_t>;
  quantvec Eup_Aup(quantvec critical_path);

  quantvec compute_critical_path(uint32_t root);

  std::ostream& to_dot(std::ostream& o);
  std::ostream& to_dot(std::ostream& o, quantvec v);
};
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree& q);
