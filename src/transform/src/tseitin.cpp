#include <cstdint>

#include <booleguru/transform/tseitin.hpp>

#include <booleguru/expression/op_manager.hpp>
#include <stdexcept>
#include <type_traits>

#include <fmt/format.h>

#include "booleguru/transform/output_to_op.hpp"
#include "booleguru/transform/output_to_qdimacs.hpp"

#include <booleguru/transform/tseitin_impl.hpp>

namespace booleguru::transform {

template class tseitin<output_to_qdimacs>;
template class tseitin<output_to_op>;
}
