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

  ankerl::unordered_dense::map<unsigned, int32_t> variable_table_;

  result parse_aag();
  result parse_aig();

  /** Save a symbol in the symbol table, remember the variable it resolves
   * to. */
  int32_t remember_symbol(int variable, const std::string& symbol);

  public:
  using base::base;
  virtual result operator()() override;
};
}
