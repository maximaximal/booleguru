#pragma once

#include <cassert>
#include <stddef.h>
#include <stdint.h>

#include <functional>

namespace booleguru::expression {
using bvop_ref_t = uint32_t;

struct bvunop {
  const bvop_ref_t l = 0;

  bvunop(bvop_ref_t l)
    : l(l) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l);
  }
  inline constexpr bvop_ref_t left() const { return l; }
  inline constexpr bvop_ref_t right() const { return 0; }
  inline constexpr bool operator==(const bvunop& o) const { return l == o.l; }
};

/** @brief Binary BV operation, also used for variables.
 *
 * A variable has width on l, and varref on r.
 */
struct bvbinop {
  const bvop_ref_t l = 0;
  const bvop_ref_t r = 0;

  bvbinop(bvop_ref_t l, bvop_ref_t r)
    : l(l)
    , r(r) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l) + 70200511 * static_cast<size_t>(r);
  }
  inline constexpr bvop_ref_t left() const { return l; }
  inline constexpr bvop_ref_t right() const { return r; }
  inline constexpr bool operator==(const bvbinop& o) const {
    return l == o.l && r == o.r;
  }
};

struct bvternop {
  const bvop_ref_t a1 = 0;
  const bvop_ref_t a2 = 0;
  const bvop_ref_t a3 = 0;

  bvternop(bvop_ref_t a1, bvop_ref_t a2, bvop_ref_t a3)
    : a1(a1)
    , a2(a2)
    , a3(a3) {}

  inline constexpr size_t hash() const {
    return (4017271 * static_cast<size_t>(a1)
            + 70200511 * static_cast<size_t>(a2))
           ^ a3;
  }
  inline constexpr bvop_ref_t left() const { return 0; }
  inline constexpr bvop_ref_t right() const { return 0; }
  inline constexpr bool operator==(const bvternop& o) const {
    return a1 == o.a1 && a2 == o.a2 && a3 == o.a3;
  }
};

// Types of binops for fixed-size bit-vectors as known from SMT-LIB:
//
// https://smtlib.cs.uiowa.edu/theories-FixedSizeBitVectors.shtml
enum class bvop_type : uint8_t {
  bv,
  bvnot,
  bvand,
  bvor,
  bvneg,
  bvadd,
  bvmul,
  bvudiv,
  bvurem,
  bvshl,
  bvlshr,
  bvult,
  concat,
  extract,
};

struct bvop {
  using ref = bvop_ref_t;
  using objtype = bvop;
  bvop_type type;
  union {
    bvunop unop;
    bvbinop binop;
    bvternop ternop;
  };

  explicit bvop(bvop_type t, ref l)
    : type(t)
    , unop(l) {
    assert(t == bvop_type::bvnot || t == bvop_type::bvneg);
  }
  explicit bvop(bvop_type t, ref l, ref r)
    : type(t)
    , binop(l, r) {
    assert(t == bvop_type::bv || t == bvop_type::bvand || t == bvop_type::bvor
           || t == bvop_type::bvadd || t == bvop_type::bvmul
           || t == bvop_type::bvudiv || t == bvop_type::bvurem
           || t == bvop_type::bvshl || t == bvop_type::bvlshr);
  }
  explicit bvop(bvop_type t, ref a1, ref a2, ref a3)
    : type(t)
    , ternop(a1, a2, a3) {}

  template<typename Functor>
  inline constexpr auto visit(Functor f) const {
    using enum bvop_type;
    switch(type) {
      case bvnot:
      case bvneg:
        return f(type, unop);
      case bv:
      case bvand:
      case bvor:
      case bvadd:
      case bvmul:
      case bvudiv:
      case bvurem:
      case bvshl:
      case bvlshr:
      case bvult:
        return f(type, binop);
      case concat:
      case extract:
        return f(type, ternop);
    }

    return f(type, unop);
  }

  inline constexpr size_t hash() const noexcept {
    return static_cast<size_t>(type) + visit([](bvop_type t, const auto& e) {
             (void)t;
             return e.hash();
           });
  }

  inline constexpr bool operator==(const bvop& o) const noexcept {
    using enum bvop_type;

    if(type != o.type)
      return false;

    switch(type) {
      case bvnot:
      case bvneg:
        return unop == o.unop;
      case bv:
      case bvand:
      case bvor:
      case bvadd:
      case bvmul:
      case bvudiv:
      case bvurem:
      case bvshl:
      case bvlshr:
      case bvult:
        return binop == o.binop;
      case concat:
      case extract:
        return ternop == o.ternop;
    }
    return false;
  }

  inline uint32_t width() const {
    assert(type == bvop_type::bv);
    return binop.l;
  }
  inline uint32_t var() const {
    assert(type == bvop_type::bv);
    return binop.r;
  }
  constexpr inline uint32_t left() const noexcept {
    return visit([](bvop_type t, const auto& e) {
      (void)t;
      return e.left();
    });
  }
  constexpr inline uint32_t right() const noexcept {
    return visit([](bvop_type t, const auto& e) {
      (void)t;
      return e.right();
    });
  }
};
}

namespace std {
template<>
struct hash<booleguru::expression::bvop> {
  size_t operator()(const booleguru::expression::bvop& x) const noexcept {
    return x.hash();
  }
};
}
