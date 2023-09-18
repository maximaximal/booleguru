#include <sstream>
#include <unordered_map>

#include <fmt/core.h>
#include <fmt/format.h>

#include <booleguru/expression/literals.hpp>
#include <booleguru/expression/op_manager.hpp>

#include <booleguru/py/model.hpp>

using map = booleguru::py::model::map;
using pair = std::pair<const map::key_type, map::value_type>;

namespace fmt {
template<>
struct formatter<::pair> {
  template<typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(const pair& fp, FormatContext& ctx) {
    booleguru::expression::op_manager& ops
      = booleguru::expression::literals::handle::global().get_op_manager();
    return format_to(ctx.out(), "\n  {} = {}", ops[fp.first], fp.second);
  }
};
}

namespace booleguru::py {

std::string
model::to_string() const {
  fmt::memory_buffer buf;
  booleguru::expression::op_manager& ops
    = booleguru::expression::literals::handle::global().get_op_manager();

  for(auto& e : values_) {
    fmt::format_to(
      std::back_inserter(buf), "{} = {}\n", ops[e.first].to_string(), e.second);
  }
  return std::string(buf.data(), buf.size());
}

}
