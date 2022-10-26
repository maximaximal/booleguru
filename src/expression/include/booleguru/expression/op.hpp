#pragma once

#include <ostream>

#include "binop.hpp"
#include "quantop.hpp"
#include "scriptop.hpp"
#include "unop.hpp"
#include "varop.hpp"

namespace booleguru::expression {
enum op_type {
  None,
  Script,
  Exists,
  Forall,
  Equi,
  Impl,
  Lpmi,
  Or,
  And,
  Not,
  Var,
};

struct op {
  // 64 bit (8 byte) user flags for ops.
  op_type type : 8;
  bool mark : 1;
  bool and_inside : 1;
  bool user_flag3 : 1;
  bool user_flag4 : 1;
  bool user_flag5 : 1;
  bool user_flag6 : 1;
  bool user_flag7 : 1;
  bool user_flag8 : 1;
  int16_t user_int16 : 16;
  int32_t user_int32 : 32;

  // Inline ops. The type is given above.
  union {
    unop un;
    binop bin;
    scriptop script;
    varop var;
    quantop quant;
  };

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
      case Exists:
      case Forall:
        quant.e = r1;
        quant.v = r2;
        break;
      case Not:
        un.c = r1;
        break;
      case And:
        and_inside = true;
        bin.l = r1;
        bin.r = r2;
        break;
      case Or:
      case Equi:
      case Impl:
      case Lpmi:
        bin.l = r1;
        bin.r = r2;
        break;
      case Script:
        script.c = r1;
        script.script_id = r2;
        break;
      case Var:
        var.v = r1;
        break;
      case None:
        break;
    }
  }

  template<typename Functor>
  inline constexpr auto visit(Functor f) const {
    switch(type) {
      case Exists:
      case Forall:
        return f(type, quant);
      case Not:
        return f(type, un);
      case And:
      case Or:
      case Equi:
      case Impl:
      case Lpmi:
        return f(type, bin);
      case Script:
        return f(type, script);
      case Var:
        return f(type, var);
      case None:
        return f(type, un);
    }

    // Never occurs, but silences compiler warnings.
    return f(type, un);
  }

  inline constexpr size_t hash() const {
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
      case Exists:
      case Forall:
        return quant == o.quant;
      case Not:
        return un == o.un;
      case And:
      case Or:
      case Equi:
      case Impl:
      case Lpmi:
        return bin == o.bin;
      case Script:
        return script == o.script;
      case Var:
        return var == o.var;
      case None:
        return true;
    }
    // Never occurs, but silences compiler warnings.
    return false;
  }

  constexpr inline uint32_t left() const {
    return visit([this](op_type t, const auto& e) {
      (void)t;
      return e.left();
    });
  }
  constexpr inline uint32_t right() const {
    return visit([this](op_type t, const auto& e) {
      (void)t;
      return e.right();
    });
  }
};

const char*
op_type_to_str(op_type t);
const char*
op_type_to_sym(op_type t);

struct op_ref;

std::ostream&
operator<<(std::ostream& o, op_type t);
}
