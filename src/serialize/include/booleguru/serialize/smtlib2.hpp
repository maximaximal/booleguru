#pragma once

#include <ostream>

#include "base.hpp"

namespace booleguru::serialize {
class smtlib2 : public base<smtlib2> {
  private:
  friend class base<smtlib2>;

  public:
  using base<smtlib2>::base;

  void operator()(expression::op_ref op);
};
}
