#pragma once

#include <ankerl/unordered_dense.h>
#include <vector>

#include "base.hpp"

namespace booleguru::parse {
class aiger : public base {
  public:
  struct gate {
    unsigned l = 0;
    unsigned r = 0;
    gate() = default;
  };

  private:
  unsigned maximum_variable_index_ = 0;
  unsigned number_of_inputs_ = 0;
  unsigned number_of_latches_ = 0;
  unsigned number_of_outputs_ = 0;
  unsigned number_of_and_gates_ = 0;

  unsigned latches_offset() const { return number_of_inputs_; }
  unsigned outputs_offset() const {
    return number_of_inputs_ + number_of_latches_;
  }
  unsigned gates_offset() const {
    return number_of_inputs_ + number_of_latches_ + number_of_outputs_;
  }

  // All variables, i.e. inputs and outputs. Gates not included.
  std::vector<uint32_t> variables;
  // All and gates. These have a gates_offset.
  std::vector<gate> gates;
  // Negations for output variables. This should only be of size 1, usually.
  std::vector<bool> negated_outputs;

  result parse_aag(std::istringstream& header);
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
