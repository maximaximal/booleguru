#pragma once

#include "base.hpp"

namespace booleguru::parse {
class luascript : public base {
  using base::base;
  virtual result operator()() override;
};
}
