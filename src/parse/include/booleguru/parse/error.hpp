#pragma once

#include <stdexcept>

namespace booleguru::parse::error {
struct generic : public std::runtime_error {
  generic(const char* what)
    : std::runtime_error(what) {}
};

struct fennel_did_not_return_op : public generic {
  fennel_did_not_return_op(const std::string& code)
    : generic(code.c_str()) {}
};
}
