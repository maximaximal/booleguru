#pragma once

#include <iosfwd>
#include <random>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/mutation.hpp>
#include <booleguru/expression/op.hpp>

#include <booleguru/util/weighted_reservoir_sampler.hpp>

namespace booleguru::expression {
class mutation_sampler {
  public:
  struct result {
    op_id id;
    mutation m;
    op o;

    result() = default;
    explicit result(op_id id, mutation m)
      : id(id)
      , m(m) {}
    explicit result(op_id id, mutation m, op o)
      : id(id)
      , m(m)
      , o(o) {}
  };

  mutation_sampler(std::minstd_rand& r)
    : s_(r) {}
  ~mutation_sampler() = default;

  void try_mutation(op_id id, mutation m) {
    s_.try_item(default_mutate_weight, result(id, m));
  }
  void try_mutation(op_id id, mutation m, op o) {
    s_.try_item(default_mutate_weight, result(id, m, o));
  }
  void try_mutation(result r) { s_.try_item(default_mutate_weight, r); }

  const result& selected() { return s_.selected(); }

  void next_generation() { s_.next_generation(); }

  private:
  const uint64_t default_mutate_weight = 1000000;
  util::weighted_reservoir_sampler<result, std::minstd_rand> s_;
};

std::ostream&
operator<<(std::ostream& o, const mutation_sampler::result& result);
}
