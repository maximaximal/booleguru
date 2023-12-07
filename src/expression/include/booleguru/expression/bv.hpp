#pragma once

#include "booleguru/util/bv_literal.hpp"
#include <cassert>
#include <functional>
#include <stddef.h>
#include <stdint.h>

#include <booleguru/expression/id.hpp>

namespace booleguru::expression {

struct bvunop {
  const bvop_id l = 0;

  bvunop(bvop_id l)
    : l(l) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l);
  }
  inline constexpr bvop_id left() const { return l; }
  inline constexpr bvop_id right() const { return 0; }
  inline constexpr bool operator==(const bvunop& o) const { return l == o.l; }
};

/** @brief Binary BV operation, also used for variables.
 *
 * A variable has width on l, and varref on r.
 */
struct bvbinop {
  const uint32_t l = 0;
  const uint32_t r = 0;

  bvbinop(bvop_id l, bvop_id r)
    : l(l)
    , r(r) {}

  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(l) + 70200511 * static_cast<size_t>(r);
  }
  inline constexpr bvop_id left() const { return l; }
  inline constexpr bvop_id right() const { return r; }
  inline constexpr bool operator==(const bvbinop& o) const {
    return l == o.l && r == o.r;
  }
};

struct bvvarop {
  const expression::var_id v;
  const uint16_t width;

  inline explicit bvvarop(expression::var_id var, uint16_t width)
    : v(var)
    , width(width) {}
  inline constexpr bool operator==(const bvvarop& o) const {
    return v == o.v && width == o.width;
  }
  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(v.id_)
           + 70200511 * static_cast<size_t>(width);
  }
  inline constexpr bvop_id left() const { return 0; }
  inline constexpr bvop_id right() const { return 0; }
};

struct bvconstop {
  const util::bv_literal lit;
  const uint16_t width;

  inline explicit bvconstop(util::bv_literal lit, uint16_t width)
    : lit(lit)
    , width(width) {}
  inline constexpr bool operator==(const bvconstop& o) const {
    return lit.n == o.lit.n && width == o.width;
  }
  inline constexpr size_t hash() const {
    return 4017271 * static_cast<size_t>(lit.n)
           + 70200511 * static_cast<size_t>(width);
  }
  inline constexpr bvop_id left() const { return 0; }
  inline constexpr bvop_id right() const { return 0; }
};

struct bvternop {
  const bvop_id a1 = 0;
  const bvop_id a2 = 0;
  const bvop_id a3 = 0;

  bvternop(bvop_id a1, bvop_id a2, bvop_id a3)
    : a1(a1)
    , a2(a2)
    , a3(a3) {}

  inline constexpr size_t hash() const {
    return (4017271 * static_cast<size_t>(a1)
            + 70200511 * static_cast<size_t>(a2))
           ^ static_cast<size_t>(a3);
  }
  inline constexpr bvop_id left() const { return 0; }
  inline constexpr bvop_id right() const { return 0; }
  inline constexpr bool operator==(const bvternop& o) const {
    return a1 == o.a1 && a2 == o.a2 && a3 == o.a3;
  }
};

// Types of binops for fixed-size bit-vectors as known from SMT-LIB:
//
// https://smtlib.cs.uiowa.edu/theories-FixedSizeBitVectors.shtml
enum class bvop_type : uint8_t {
  bvvar,
  bvconst,
  and_,
  or_,
  not_,
  implies,
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
  bveq,
  bvforall,
  bvexists,
  concat,
  extract,
  ite,
};

const char*
bvop_type_to_str(bvop_type t) noexcept;

struct bvop {
  using objtype = bvop;
  using id = bvop_id;

  bvop_type type;
  union {
    bvvarop varop;
    bvconstop constop;
    bvunop unop;
    bvbinop binop;
    bvternop ternop;
  };

  explicit bvop(bvop_type t, id l)
    : type(t)
    , unop(l) {
    assert(t == bvop_type::not_ | t == bvop_type::bvnot
           || t == bvop_type::bvneg);
  }
  explicit bvop(bvop_type t, id l, id r)
    : type(t)
    , binop(l, r) {
    assert(is_binop(t));
  }
  explicit bvop(bvop_type t, id a1, id a2, id a3)
    : type(t)
    , ternop(a1, a2, a3) {}
  explicit bvop(bvop_type t, expression::var_id v, uint16_t width)
    : type(t)
    , varop(v, width) {
    assert(t == bvop_type::bvvar);
  }
  explicit bvop(bvop_type t, util::bv_literal lit, uint16_t width)
    : type(t)
    , constop(lit, width) {
    assert(t == bvop_type::bvconst);
  }

  static bool is_binop(bvop_type t) {
    using enum bvop_type;
    return t == bveq || t == bvforall || t == bvexists || t == and_ || t == or_
           || t == bvand || t == implies || t == bvor || t == bvadd
           || t == bvmul || t == bvudiv || t == bvurem || t == bvshl
           || t == bvlshr || t == bvult;
  }

  static bool is_ternop(bvop_type t) {
    using enum bvop_type;
    return t == concat || t == extract || t == ite;
  }

  template<typename Functor>
  inline constexpr auto visit(Functor f) const {
    using enum bvop_type;
    switch(type) {
      case bvvar:
        return f(type, varop);
      case bvconst:
        return f(type, constop);
      case not_:
      case bvnot:
      case bvneg:
        return f(type, unop);
      case and_:
      case or_:
      case implies:
      case bvand:
      case bvor:
      case bvadd:
      case bvmul:
      case bvudiv:
      case bvurem:
      case bvshl:
      case bvlshr:
      case bvult:
      case bveq:
      case bvforall:
      case bvexists:
        return f(type, binop);
      case concat:
      case extract:
      case ite:
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
      case bvvar:
        return varop == o.varop;
      case bvconst:
        return constop == o.constop;
      case not_:
      case bvnot:
      case bvneg:
        return unop == o.unop;
      case and_:
      case or_:
      case implies:
      case bvand:
      case bvor:
      case bvadd:
      case bvmul:
      case bvudiv:
      case bvurem:
      case bvshl:
      case bvlshr:
      case bvult:
      case bveq:
      case bvforall:
      case bvexists:
        return binop == o.binop;
      case concat:
      case extract:
      case ite:
        return ternop == o.ternop;
    }
    return false;
  }

  constexpr inline bvop_id left() const noexcept {
    return visit([](bvop_type t, const auto& e) {
      (void)t;
      return e.left();
    });
  }
  constexpr inline bvop_id right() const noexcept {
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
