#pragma once

#include <ostream>

#include "base.hpp"

#include <booleguru/transform/output_to_qdimacs.hpp>
#include <booleguru/transform/tseitin.hpp>

namespace booleguru::serialize {
class qdimacs : public base<qdimacs> {
  private:
  friend class base<qdimacs>;

  public:
  using base<qdimacs>::base;

  void operator()(expression::op_ref op) override {
    if(op->is_prenex) {
      if(op->is_cnf) {
        transform::output_to_qdimacs transformer(o_);
        transformer.serialize_cnf_op(op);
      } else {
        transform::tseitin<transform::output_to_qdimacs> transformer(o_);
        transformer(op);
      }
    } else {
      throw std::runtime_error("Non-prenex formulas not supported in"
                               " QDIMACS output.");
    }
  };
};
}
