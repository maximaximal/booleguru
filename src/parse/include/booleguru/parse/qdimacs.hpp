#pragma once

#include <vector>

#include "base.hpp"

namespace booleguru::parse {
class qdimacs : public base {
  bool read_header(std::istringstream& line);

  enum phase {
    header,
    prefix_and_matrix,
  };

  phase phase_ = header;

  int varcount_ = -1;
  int clausecount_ = -1;

  enum quantifier {
    exists,
    forall,
  };
  std::vector<std::pair<quantifier, int>> quantified_;

  public:
  using base::base;
  virtual result operator()() override;
};
}
