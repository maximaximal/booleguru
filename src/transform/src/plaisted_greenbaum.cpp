#include <cstdint>

#include <booleguru/transform/plaisted_greenbaum.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <stdexcept>
#include <type_traits>

#include <fmt/format.h>

#include "booleguru/transform/output_to_op.hpp"
#include "booleguru/transform/output_to_qdimacs.hpp"

#include <booleguru/transform/plaisted_greenbaum_impl.hpp>

namespace booleguru::transform {

template class plaisted_greenbaum<output_to_qdimacs>;
template class plaisted_greenbaum<output_to_op>;
}
