#pragma once

#include <stdexcept>
namespace booleguru::util {
struct unsupported : public std::runtime_error {
  unsupported(const char* what)
    : std::runtime_error(what) {}
  unsupported(std::string what)
    : std::runtime_error(what) {}
};
}
