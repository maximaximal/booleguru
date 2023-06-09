#pragma once

#include <string>
#include <string_view>
#include <variant>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::py {
using python_result = std::variant<std::string, expression::op_ref>;
python_result
eval(std::istream& in);
}
