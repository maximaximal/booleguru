#pragma once

#include <ankerl/unordered_dense.h>
#include <vector>

#include "base.hpp"

namespace booleguru::parse {
class aiger : public base {
  unsigned maximum_variable_index_ = 0;
  unsigned number_of_inputs_ = 0;
  unsigned number_of_latches_ = 0;
  unsigned number_of_outputs_ = 0;
  unsigned number_of_and_gates_ = 0;

  unsigned latches_offset() const { return number_of_inputs_; }
  unsigned outputs_offset() const {
    return number_of_inputs_ + number_of_latches_;
  }

  struct and_ {
    unsigned l = 0;
    unsigned r = 0;
  };
  struct var_ {
    // At first, this is just 0 or 1 (1 means notted variable). Once symbols are
    // added, variables are resolved to entries in the variable manager.
    unsigned var_ = 0;
    inline unsigned notted() const { return var_ & 0b1; }
  };
  struct gate {
    union {
      and_ a;
      var_ v;
    };
    inline bool is_and() const { return a.r == 0; }
    inline bool is_var() const { return a.r != 0; }
  };
  std::vector<gate> gates_;

  result parse_aag();
  result parse_aig();

  /** Save a symbol in the symbol table, remember the variable it resolves
   * to. */
  void remember_symbol(unsigned varop, const std::string& symbol);

  result build();

  public:
  using base::base;
  virtual result operator()() override;
};
}
