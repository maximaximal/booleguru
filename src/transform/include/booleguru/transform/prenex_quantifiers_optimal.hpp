#pragma once

#include <memory>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::transform {
struct prenex_quantifier_optimal {
  enum kind : uint8_t {
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
  struct pass2;

  std::string animation_path_;
  bool animate_ = false;
  uint32_t animation_step_ = 0;

  using node_ptr = std::shared_ptr<prenex_quantifier_optimal::node>;

  std::unique_ptr<inner> i;

  bool encountered_quant_ = false;
  const kind kind_;

  enum dir { up, down };

  expression::op_type extract_prioritized(kind k) const {
    switch(k) {
      case Eup_up:
      case Eup_down:
      case Edown_up:
      case Edown_down:
        return expression::op_type::Exists;
      case Aup_up:
      case Aup_down:
      case Adown_up:
      case Adown_down:
        return expression::op_type::Forall;
    }
    assert(false);
  }
  dir extract_d1(kind k) const {
    switch(k) {
      case Eup_up:
      case Eup_down:
      case Aup_up:
      case Aup_down:
        return up;
      case Edown_up:
      case Edown_down:
      case Adown_up:
      case Adown_down:
        return down;
    }
    assert(false);
  }
  dir extract_d2(kind k) const {
    switch(k) {
      case Eup_up:
      case Aup_up:
      case Adown_up:
      case Edown_up:
        return up;
      case Edown_down:
      case Adown_down:
      case Eup_down:
      case Aup_down:
        return down;
    }
    assert(false);
  }

  const expression::op_type prioritized_ = extract_prioritized(kind_);
  const dir d1_ = extract_d1(kind_);
  const dir d2_ = extract_d2(kind_);

  /// Pre-process the internal tree, such that all node objects in the tree are
  /// of alternating quantifier type. Modifies i->s.
  void preprocess(node_ptr& root);

  /// Assign depths and heights to nodes.
  uint32_t assign_height_depth(node& n, uint32_t h = 1);

  /// Extract the critical path into i->critical_path.
  void extract_critical_path(const node_ptr& root);

  /// Pass 1, compute f
  void pass1(const node_ptr& root);

  /// Pass 2, compute g
  /// use the pass2 struct.

  /// Prenex the quantifiers according to kind_. Modifies i->s.
  void prenex(node_ptr root);

  uint32_t f(node& n);
  uint32_t f_down(node& n);
  uint32_t f_up(node& n);

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
