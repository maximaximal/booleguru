#pragma once

#include <cassert>
#include <cstdint>
#include <ostream>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
class output_to_qdimacs {
  std::ostream& o;

  bool in_exists = false;
  bool in_forall = false;
  bool in_prefix = true;

  void inline maybe_end_exists() {
    if(in_exists) {
      o << "0\n";
      in_exists = false;
    }
  }

  void inline maybe_end_forall() {
    if(in_forall) {
      o << "0\n";
      in_forall = false;
    }
  }

  void inline maybe_end_quant() {
    assert(!(in_exists && in_forall));
    maybe_end_exists();
    maybe_end_forall();
  }

  public:
  using initarg = std::ostream&;
  using TransformResult = void;

  explicit output_to_qdimacs(initarg o)
    : o(o) {}

  void problem(int32_t variables, int32_t clauses) {
    o << "p cnf " << variables << " " << clauses << "\n";
  }

  using ref = int32_t;
  void exists(ref x) {
    assert(in_prefix);
    maybe_end_forall();
    if(!in_exists) {
      o << "e ";
      in_exists = true;
    }
    o << x << " ";
  }
  void forall(ref x) {
    assert(in_prefix);
    maybe_end_exists();
    if(!in_forall) {
      o << "a ";
      in_forall = true;
    }
    o << x << " ";
  }
  void end_prefix() {
    maybe_end_quant();
    in_prefix = false;
  };
  void unit(ref x1) {
    assert(!in_prefix);
    o << x1 << " 0\n";
  }
  void binary(ref x1, ref x2) {
    assert(!in_prefix);
    o << x1 << " " << x2 << " 0\n";
  }
  void ternary(ref x1, ref x2, ref x3) {
    assert(!in_prefix);
    o << x1 << " " << x2 << " " << x3 << " 0\n";
  }

  TransformResult get_out() const {}

  inline constexpr ref op_ref_to_ref(const expression::op& o,
                                     expression::op_ref::ref id) {
    (void)id;
    return o.user_int32;
  }

  inline ref not_op(ref r) { return -r; }
};
}
