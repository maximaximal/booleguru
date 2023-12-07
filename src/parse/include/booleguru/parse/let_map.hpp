#pragma once

#include <ankerl/unordered_dense.h>
#include <string>
#include <vector>

#include <booleguru/expression/bv.hpp>
#include <booleguru/expression/id.hpp>

namespace booleguru::parse {
struct binding_conflict : std::runtime_error {
  binding_conflict(std::string id)
    : std::runtime_error(
      fmt::format("Two bindings with same ID {} in same let encountered", id)) {
  }
};

class let_map {
  using bvop_map
    = ankerl::unordered_dense::map<std::string, expression::bvop_id>;
  using lets_vec = std::vector<bvop_map>;
  lets_vec lets_;

  public:
  let_map() = default;
  ~let_map() = default;

  expression::bvop_id operator[](std::string id) const;

  void add(std::string id, expression::bvop_id op);

  void push();
  void pop();
};
}
