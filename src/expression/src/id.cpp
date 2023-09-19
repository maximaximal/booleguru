// Author: Marcel Simader (marcel.simader@jku.at)
// Date: 14.09.2023

#include <booleguru/expression/id.hpp>

using namespace booleguru;

#define STATIC_ASSERT_ID_SIZE(id_type)                    \
  static_assert(sizeof(id_type) == sizeof(uint32_t),      \
                "Ensure that size of ID type ('" #id_type \
                "') does not exceed the size of a "       \
                "single 'uint32_t'.")

namespace booleguru::expression {
STATIC_ASSERT_ID_SIZE(var_id);
STATIC_ASSERT_ID_SIZE(op_id);
STATIC_ASSERT_ID_SIZE(bvop_id);
STATIC_ASSERT_ID_SIZE(script_id);
};

