#include <booleguru/expression/op_manager.hpp>
#include <booleguru/transform/cnf.hpp>
#include <booleguru/transform/distribute_nots.hpp>
#include <booleguru/transform/distribute_ors.hpp>
#include <booleguru/transform/eliminate_equivalence.hpp>
#include <booleguru/transform/eliminate_implication.hpp>
#include <booleguru/transform/eliminate_xor.hpp>

namespace booleguru::transform {
expression::op_ref
distribute_to_cnf(expression::op_ref r) {
  return distribute_ors()(distribute_nots()(
    eliminate_equivalence()(eliminate_implication()(eliminate_xor()(r)))));
}
}
