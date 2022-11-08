#pragma once

#include <map>
#include <optional>
#include <string>

namespace booleguru::expression {
class op_ref;
}

typedef union cl_lispunion* cl_object;

namespace booleguru::cl {
std::optional<cl_object>
cl_object_conv(const cl_object& src, expression::op_ref& tgt);

cl_object
make_error(std::string symbol,
           std::map<std::string, std::string> properties = {});


}
