#pragma once

#include <ostream>

#include "base.hpp"

namespace booleguru::serialize {
class qcir : public base<qcir> {
  private:
  friend class base<qcir>;

  inline void walk_exists(op_ref o) { walk_quant(o); }
  inline void walk_forall(op_ref o) { walk_quant(o); }
  void walk_not(op_ref o);
  inline void walk_and(op_ref o) { walk_nargsop("and", o); }
  inline void walk_or(op_ref o) { walk_nargsop("or", o); }
  void walk_equi(op_ref o);
  void walk_impl(op_ref o);
  void walk_lpmi(op_ref o);
  void walk_xor(op_ref o);
  void walk_var(op_ref o);

  void walk_quant(op_ref o);
  std::vector<int32_t> walk_nargsop(std::string_view gatetype,
                                    op_ref o,
                                    bool last = true);

  bool on_quant_prefix_ = true;
  bool cleansed_ = false;

  public:
  using base<qcir>::base;

  void operator()(expression::op_ref op);
};
}
