#pragma once

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/manager.hpp>

#include <booleguru/transform/exact-1.hpp>

namespace booleguru::parse {
struct uvl_subfeature {
  using op_id = booleguru::expression::op_id;

  uvl_subfeature(std::shared_ptr<expression::op_manager> ops, op_id parent)
    : ops(ops)
    , f(parent) {}

  virtual ~uvl_subfeature() = default;

  virtual void add(op_id sub) = 0;
  virtual op_id finalize() = 0;

  protected:
  std::shared_ptr<expression::op_manager> ops;
  op_id f;
};

struct uvl_subfeature_mandatory : public uvl_subfeature {
  using uvl_subfeature::uvl_subfeature;
  op_id curr = 0;
  virtual void add(op_id sub) override {
    curr = ops->encode_conjunct(curr, ops->encode_equi(sub, f));
  }
  virtual op_id finalize() override { return curr; }
};

struct uvl_subfeature_optional : public uvl_subfeature {
  using uvl_subfeature::uvl_subfeature;
  op_id curr = 0;
  virtual void add(op_id sub) override {
    curr = ops->encode_conjunct(curr, ops->encode_impl(sub, f));
  }
  virtual op_id finalize() override { return curr; }
};

struct uvl_subfeature_or : public uvl_subfeature {
  using uvl_subfeature::uvl_subfeature;
  op_id curr = 0;
  virtual void add(op_id sub) override {
    curr = ops->encode_disjunct(curr, sub);
  }
  virtual op_id finalize() override { return ops->encode_equi(curr, f); }
};

struct uvl_subfeature_xor : public uvl_subfeature {
  transform::exact_1 ex1;

  uvl_subfeature_xor(std::shared_ptr<expression::op_manager> ops, op_id parent)
    : uvl_subfeature(ops, parent)
    , ex1(*ops) {}

  virtual void add(op_id sub) override { ex1.add(sub); }
  virtual op_id finalize() override {
    op_id disj = 0;
    for(op_id sub : ex1.subs) {
      disj = ops->encode_disjunct(disj, sub);
    }

    assert(f);
    assert(disj);
    
    return ops->encode_and(ops->encode_impl(disj, f),
                           ops->encode_impl(f, ex1.finalize()));
  }
};

}
