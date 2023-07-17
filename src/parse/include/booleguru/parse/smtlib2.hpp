#pragma once

#include "base.hpp"

namespace booleguru::parse {
class smtlib2 : public base {
  protected:
  struct internal;
  struct internal_deleter {
    void operator()(internal* i);
  };
  std::unique_ptr<internal, internal_deleter> internal_;

  virtual void init() override;

  public:
  virtual ~smtlib2();

  using base::base;
  virtual result operator()() override;
};
}
