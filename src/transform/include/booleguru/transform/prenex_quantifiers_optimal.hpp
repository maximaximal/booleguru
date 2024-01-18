#pragma once

#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
struct prenex_quantifier_optimal {
  enum kind {
    Eup_up,
    Eup_down,
    Edown_up,
    Edown_down,
    Aup_up,
    Aup_down,
    Adown_up,
    Adown_down,
  };

  prenex_quantifier_optimal(kind k = Eup_up);
  ~prenex_quantifier_optimal();

  void animate(std::string_view path) {
    animation_path_ = path;
    animate_ = path != "";
  }

  expression::op_ref operator()(expression::op_ref o);

  expression::op_ref rebind_variable(expression::op_ref o,
                                     expression::op_ref bound_v);

  private:
  struct node;
  struct inner;

  std::string animation_path_;
  bool animate_ = false;
  uint32_t animation_step_ = 0;

  using node_ptr = std::shared_ptr<prenex_quantifier_optimal::node>;

  std::unique_ptr<inner> i;

  bool encountered_quant_ = false;
  const kind kind_;

  /// Pre-process the internal tree, such that all node objects in the tree are
  /// of alternating quantifier type. Modifies i->s.
  void preprocess(node_ptr root);

  /// Assign depths and heights to nodes.
  uint32_t assign_height_depth(node& n, uint32_t h = 1);

  /// Extract the critical path into i->critical_path.
  void extract_critical_path(const node_ptr &root);

  /// Prenex the quantifiers according to kind_. Modifies i->s.
  void prenex(node_ptr root);

  void conditionally_create_animation_step(expression::op_manager& mgr,
                                           const node_ptr& root);
  void to_dot(expression::op_manager& mgr,
              const node_ptr& root,
              std::ostream& o);

  void emplace_l_r(node_ptr& l, node_ptr& r);

  expression::op_ref walk(expression::op_ref o);

  expression::op_ref walk_quant(expression::op_ref o);

  expression::op_ref walk_not(expression::op_ref o);

  expression::op_ref walk_impl(expression::op_ref o);

  expression::op_ref walk_lpmi(expression::op_ref o);

  expression::op_ref walk_equi(expression::op_ref o);

  expression::op_ref walk_bin(expression::op_ref o);

  expression::op_ref walk_xor(expression::op_ref o);
};
}
