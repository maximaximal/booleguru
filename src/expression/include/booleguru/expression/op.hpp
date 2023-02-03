#pragma once

#include <ostream>

#include "binop.hpp"
#include "quantop.hpp"
#include "scriptop.hpp"
#include "unop.hpp"
#include "varop.hpp"

namespace booleguru::expression {
enum class op_type {
  None,
  Exists,
  Forall,
  Equi,
  Impl,
  Lpmi,
  Or,
  And,
  Xor,
  Not,
  Var,
};

[[nodiscard]] inline op_type
op_type_flip_quantifier(op_type in) noexcept {
  switch(in) {
    case op_type::Exists:
      return op_type::Forall;
    case op_type::Forall:
      return op_type::Exists;
    default:
      return op_type::None;
  }
}

struct op {
  op_type type : 8;
  // 56 bit (fill up type to 8 byte) user flags for ops. These are not included
  // in the hashing of ops and are therefore left to be mutable.
  mutable bool mark : 1;
  mutable bool and_inside : 1;
  mutable bool user_flag3 : 1;
  mutable bool user_flag4 : 1;
  mutable bool user_flag5 : 1;
  mutable bool user_flag6 : 1;
  mutable bool user_flag7 : 1;
  mutable bool user_flag8 : 1;
  mutable int16_t user_int16 : 16;
  mutable int32_t user_int32 : 32;

  // Inline ops. The type is given above.
  union {
    unop un;
    binop bin;
    varop var;
    quantop quant;
  };

  inline explicit constexpr op() {}

  inline explicit constexpr op(op_type type, uint32_t r1, uint32_t r2)
    : type(type)
    , mark(false)
    , and_inside(false)
    , user_flag3(false)
    , user_flag4(false)
    , user_flag5(false)
    , user_flag6(false)
    , user_flag7(false)
    , user_flag8(false)
    , user_int16(0)
    , user_int32(0)
    , bin(0, 0) {

    switch(type) {
      case op_type::Exists:
      case op_type::Forall:
        quant.v = r1;
        quant.e = r2;
        break;
      case op_type::Not:
        un.c = r1;
        break;
      case op_type::And:
        and_inside = true;
        bin.l = r1;
        bin.r = r2;
        break;
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        bin.l = r1;
        bin.r = r2;
        break;
      case op_type::Var:
        var.v = r1;
        var.q = r2;
        break;
      case op_type::None:
        break;
    }
  }

  template<typename Functor>
  inline constexpr auto visit(Functor f) const {
    switch(type) {
      case op_type::Exists:
      case op_type::Forall:
        return f(type, quant);
      case op_type::Not:
        return f(type, un);
      case op_type::And:
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        return f(type, bin);
      case op_type::Var:
        return f(type, var);
      case op_type::None:
        return f(type, un);
    }

    // Never occurs, but silences compiler warnings.
    return f(type, un);
  }

  inline constexpr size_t hash() const noexcept {
    return static_cast<size_t>(type) + visit([](op_type t, const auto& e) {
             (void)t;
             return e.hash();
           });
  }

  constexpr inline size_t operator()() const { return hash(); }

  constexpr inline bool operator==(const op& o) const {
    if(type != o.type)
      return false;

    switch(type) {
      case op_type::Exists:
      case op_type::Forall:
        return quant == o.quant;
      case op_type::Not:
        return un == o.un;
      case op_type::And:
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        return bin == o.bin;
      case op_type::Var:
        return var == o.var;
      case op_type::None:
        return true;
    }
    // Never occurs, but silences compiler warnings.
    return false;
  }

  constexpr inline uint32_t left() const {
    return visit([](op_type t, const auto& e) {
      (void)t;
      return e.left();
    });
  }
  constexpr inline uint32_t right() const {
    return visit([](op_type t, const auto& e) {
      (void)t;
      return e.right();
    });
  }
};

const char*
op_type_to_str(op_type t);
const char*
op_type_to_sym(op_type t);

class op_ref;

std::ostream&
operator<<(std::ostream& o, op_type t);
}

// This was tested against fully hashing with the hash provided by Ankerl, but
// the results were worse compared to Limboole's hash values.
namespace std {
template<>
struct hash<booleguru::expression::op> {
  size_t operator()(const booleguru::expression::op& x) const noexcept {
    return x.hash();
  }
};
}
