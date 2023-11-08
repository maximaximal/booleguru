#pragma once

#include <cstddef>

#include "mutation_sampler.hpp"

namespace booleguru::expression {
struct op;

class mutator {
  mutation_sampler s_;

  public:
  mutator(std::minstd_rand& r)
    : s_(r) {}
  ~mutator() = default;

  void mutate(op* ops, size_t& size, size_t capacity);
};
}
