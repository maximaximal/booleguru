#pragma once

#include "base.hpp"

namespace booleguru::parse {
class pythonscript : public base {
  using base::base;
  virtual result operator()() override;
};
}
