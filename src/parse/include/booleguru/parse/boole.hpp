#pragma once

#include <optional>
#include <variant>

#include "base.hpp"
#include "sexpr-tracker.hpp"

namespace booleguru::parse {
class boole : public base {
  virtual void init() override;

  public:
  using base::base;

  virtual ~boole();

  void eval(bool enable);

  virtual result operator()() override;

  protected:
  struct internal;
  struct internal_deleter {
    void operator()(internal*);
  };
  std::unique_ptr<internal, internal_deleter> internal_;
};
}
