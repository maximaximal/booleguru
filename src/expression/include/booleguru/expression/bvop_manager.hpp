#pragma once

#include "manager.hpp"
#include "reference.hpp"

#include "bv.hpp"

namespace booleguru::expression {
class op_manager;
class bvop_manager;

class bvop_ref : public reference<bvop, bvop_manager> {
  public:
  using reference<bvop, bvop_manager>::reference;

  bvop_ref left();
  bvop_ref right();

  op_ref export_as_ops(op_manager& ops);
};

class bvop_manager : public manager<bvop_ref, bvop_manager> {
  public:
  using base = manager<bvop_ref, bvop_manager>;
};

}
