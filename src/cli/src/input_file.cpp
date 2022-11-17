#include <booleguru/cli/input_file.hpp>

#include <booleguru/expression/op_manager.hpp>

namespace booleguru::cli {
input_file::input_file(std::string_view path, std::vector<argument>&& args)
  : path_(path)
  , args_(std::move(args)) {}

expression::op_ref
input_file::process() {
  return expression::op_ref();
}
}
