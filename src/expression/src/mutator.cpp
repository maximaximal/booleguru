#include <booleguru/expression/id.hpp>
#include <booleguru/expression/mutation.hpp>
#include <booleguru/expression/mutation_sampler.hpp>
#include <booleguru/expression/mutator.hpp>
#include <booleguru/expression/op.hpp>

#include <bitset>
#include <iostream>

namespace booleguru::expression {
using mutation_bitset = std::bitset<static_cast<size_t>(mutation::last) + 1>;

static void
gen_possibilities(op_id id, op& o, mutation mut, mutation_sampler& s) {
  using enum op_type;
  using enum mutation;
  op o_;

  // All unops
  for(op_id c{ 0 }; c < id; ++c) {
    o_.type = Not;
    o_.un.c = c;
    if(o != o_)
      s.try_mutation(id, mut, o_);
  }

  // All binops
  for(op_id l{ 0 }; l < id; ++l) {
    for(op_id r{ 0 }; r < id; ++r) {
      for(uint8_t m = static_cast<uint8_t>(Equi);
          m <= static_cast<uint8_t>(Xor);
          ++m) {
        op_type t = static_cast<op_type>(m);
        o_.type = t;
        o_.bin.l = l;
        o_.bin.r = r;
        if(o != o_)
          s.try_mutation(id, mut, o_);
      }
    }
  }

  // Varops. These can be endless, so we limit them to the ID count.
  for(var_id v{ 1 }; v.id_ <= id.id_ + 1; ++v) {
    o_.type = Var;
    o_.var.v = v;
    o_.var.q = 0;
    o_.var.i = 0;
    if(o != o_)
      s.try_mutation(id, mut, o_);
  }
}

void
mutator::mutate(op* ops, size_t& size, size_t capacity) {
  using enum mutation;
  using enum op_type;

  assert(capacity > 0);

  s_.next_generation();

  if(size > 0) {
    s_.try_mutation(size - 1, pop);
  }
  for(size_t i = 0; i < size; ++i) {
    gen_possibilities(i, ops[i], change, s_);
  }
  if(size < capacity) {
    gen_possibilities(size, ops[size], push, s_);
  }

  switch(s_.selected().m) {
    case pop:
      --size;
      break;
    case push:
      ++size;
      [[fallthrough]];
    case change:
      assert(s_.selected().id.id_ < capacity);
      ops[s_.selected().id.id_] = s_.selected().o;
      break;
  }
}
}
