#pragma once

#include <booleguru/util/bv_literal.hpp>

#include <booleguru/expression/bv.hpp>
#include <booleguru/expression/manager.hpp>
#include <booleguru/expression/reference.hpp>

namespace booleguru::expression {
class op_manager;
class bvop_manager;

class bvop_ref : public reference<bvop, bvop_manager, bvop_id> {
  public:
  using reference<bvop, bvop_manager, bvop_id>::reference;

  bvop_ref left();
  bvop_ref right();

  op_ref export_as_ops(op_manager& ops);

  bvop_id encode_bv_literal(util::bv_literal lit);
};

class bvop_manager : public manager<bvop_ref, bvop_manager> {
  public:
  using base = manager<bvop_ref, bvop_manager>;

  void render_as_dot(std::ostream& o, bvop_id id) const noexcept;
};

}
