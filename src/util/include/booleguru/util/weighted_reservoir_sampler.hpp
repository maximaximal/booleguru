#pragma once

#include <random>

namespace booleguru::util {
// Pick one item from a sequence of weighted items. Very inspired from google's
// https://github.com/google/libprotobuf-mutator project (Apache 2 License)
template<class T, class R = std::default_random_engine>
class weighted_reservoir_sampler {
  R& r_;
  T selected_;
  uint64_t total_weight_ = 0;

  bool pick(uint64_t weight) {
    if(weight == 0)
      return false;

    total_weight_ += weight;
    return weight == total_weight_
           || std::uniform_int_distribution<uint64_t>(1, total_weight_)(r_)
                <= weight;
  }

  public:
  weighted_reservoir_sampler(R& r)
    : r_(r) {}

  T& selected() { return selected_; }

  void next_generation() { total_weight_ = 0; };

  void try_item(uint64_t weight, const T& item) {
    if(pick(weight))
      selected_ = item;
  }
};
}
