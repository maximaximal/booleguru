#pragma once

#include "base.hpp"

namespace booleguru::parse {

class uvl : public base {

  struct internal;
  struct internal_deleter {
    void operator()(internal*);
  };
  std::unique_ptr<internal, internal_deleter> internal_;

  virtual void init() override;

  public:
  using base::base;
  virtual ~uvl();
  virtual result operator()() override;
};

}
