#pragma once

#include <cstdint>
#include <stack>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
class output_to_op {
  public:
  using id = expression::op_id;

  private:
  expression::op_ref o;
  expression::op_manager& mgr;
  expression::var_id tseitin_id = mgr.vars().LITERAL_TSEITIN;

  std::stack<std::pair<expression::op_type, expression::op_id>> quants;

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
  explicit output_to_op(initarg& mgr)
    : mgr(mgr) {}

  void problem(int32_t variables, int32_t clauses) {
    (void)variables;
    (void)clauses;
  }

  void exists(expression::op_id x) {
    quants.push(std::make_pair(expression::op_type::Exists, x));
  }
  void forall(expression::op_id x) {
    quants.push(std::make_pair(expression::op_type::Forall, x));
  }
  void end_prefix() {}
  void unit(expression::op_id x1) { o = and_concat(mgr[x1]); }
  void binary(expression::op_id x1, expression::op_id x2) {
    o = and_concat(mgr[x1] || mgr[x2]);
  }
  void ternary(expression::op_id x1,
               expression::op_id x2,
               expression::op_id x3) {
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

  inline constexpr expression::op_id op_ref_to_ref(const expression::op o,
                                                   expression::op_id id) {
    if(o.type == expression::op_type::Var) {
      return id;
    } else {
      return mgr.get_id(
        expression::op(expression::op_type::Var, tseitin_id, (uint16_t)id));
    }
  }

  inline expression::op_id not_op(expression::op_id id) {
    return mgr.get_id(expression::op(expression::op_type::Not, id, 0));
  }

  void insert_mapping_comment(expression::op_id id, std::string_view name) {
    (void)id;
    (void)name;
  }
};
}
