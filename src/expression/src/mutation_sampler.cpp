#include <iostream>

#include <booleguru/expression/mutation.hpp>
#include <booleguru/expression/mutation_sampler.hpp>

namespace booleguru::expression {
std::ostream&
operator<<(std::ostream& o, const mutation_sampler::result& r) {
  o << "Id:" << r.id.id_ << ", M:" << r.m;
  if(r.m != mutation::pop)
    o << ", Op:" << r.o;
  return o;
}
}
