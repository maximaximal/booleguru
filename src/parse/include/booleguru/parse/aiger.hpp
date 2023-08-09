#pragma once

#include <ankerl/unordered_dense.h>
#include <vector>

#include "base.hpp"

namespace booleguru::parse {
class aiger : public base {
  public:
  struct gate {
    bool inverted = false;
    unsigned l = 0;
    unsigned r = 0;
    uint32_t var_id;
    inline bool is_and() const { return r != 0; }
    inline bool is_var() const { return r == 0; }

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

  std::vector<gate> gates_;

  unsigned left(unsigned var) const;
  unsigned right(unsigned var) const;

  result parse_aag(std::istringstream &header);
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
