#pragma once

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/manager.hpp>

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
  op_id disj = 0;

  using uvl_subfeature::uvl_subfeature;

  std::vector<op_id> subs;

  virtual void add(op_id sub) override {
    disj = ops->encode_disjunct(disj, sub);
    subs.emplace_back(sub);
  }
  virtual op_id finalize() override {
    op_id rhs = 0;
    for(size_t i = 0; i < subs.size(); ++i) {
      for(size_t j = 0; j < subs.size(); ++j) {
        if(i == j)
          continue;
        rhs = ops->encode_conjunct(
          rhs, ops->encode_not(ops->encode_and(subs[i], subs[j])));
      }
    }
    return ops->encode_and(ops->encode_equi(disj, f), rhs);
  }
};

}
