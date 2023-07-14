#pragma once

#include "base.hpp"

namespace booleguru::parse {
class smtlib2 : public base {
  public:
  using base::base;
  virtual result operator()() override;
};
}
