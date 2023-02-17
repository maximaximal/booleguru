#pragma once

#include <cstdint>
#include <stack>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
class output_to_op {
  public:
  using ref = typename expression::op_manager::ref;

  private:
  expression::op_ref o;
  expression::op_manager& mgr;

  std::stack<std::pair<expression::op_type, ref>> quants;

  expression::op_ref inline and_concat(expression::op_ref r) {
    if(o.valid()) {
      return o && r;
    } else {
      return r;
    }
  }

  public:
  using initarg = expression::op_manager&;
  using TransformResult = expression::op_ref;
  explicit output_to_op(initarg mgr)
    : mgr(mgr) {}

  void problem(int32_t variables, int32_t clauses) {
    (void)variables;
    (void)clauses;
  }

  void exists(ref x) {
    quants.push(std::make_pair(expression::op_type::Exists, x));
  }
  void forall(ref x) {
    quants.push(std::make_pair(expression::op_type::Forall, x));
  }
  void end_prefix() {}
  void unit(ref x1) { o = and_concat(mgr[x1]); }
  void binary(ref x1, ref x2) { o = and_concat(mgr[x1] || mgr[x2]); }
  void ternary(ref x1, ref x2, ref x3) {
    o = and_concat(mgr[x1] || mgr[x2] || mgr[x3]);
  }

  TransformResult get_out() {
    while(!quants.empty()) {
      auto [t, q] = quants.top();
      o = mgr.get(expression::op(t, q, o.get_id()));
      quants.pop();
    }
    return o;
  }

  static inline constexpr ref op_ref_to_ref(const expression::op o,
                                            expression::op_ref::ref id,
                                            expression::op_manager& mgr) {
    if(o.type == expression::op_type::Var) {
      return id;
    } else {
      ref tseitin_id = mgr.vars().get_id(expression::variable{ "tseitin" });
      return mgr.get_id(
        expression::op(expression::op_type::Var, tseitin_id, id));
    }
  }
};
}
