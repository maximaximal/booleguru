#pragma once

#include <stdexcept>
#include <string>
#include <string_view>

#include <fmt/format.h>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::parse {
struct smtlib2_variable_already_defined_error : std::runtime_error {
  smtlib2_variable_already_defined_error(std::string_view variable)
    : std::runtime_error(
      fmt::format("Variable {} already defined!", variable)) {}
};

struct smtlib2_variable {
  expression::var_id id;
  uint16_t width;

  // Even though there are two different types of smt variables, all variables
  // are encoded as BitVec variables internally, represented by the BitVec
  // structure. This collapses to the same boolean encoding in the end. The
  // information about a variable's sort is in a formula though, so it must be
  // saved here, in order to enable error reporting.
  enum type : uint8_t { BitVec, Bool } t;

  smtlib2_variable(expression::var_id id, type t, uint16_t w)
    : id(id)
    , width(w)
    , t(t) {}

  template<class M>
  static smtlib2_variable& define_variable(M& map,
                                           expression::var_manager& vars,
                                           const std::string& v,
                                           type t,
                                           uint16_t w = 1) {
    assert((t == type::Bool && w == 1) || (t == type::BitVec));
    auto it = map.find(v);
    if(it != map.end()) {
      throw smtlib2_variable_already_defined_error(v);
    }

    auto [inserted_it, _] = map.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(v),
      std::forward_as_tuple(vars.get_id(expression::variable{ v }), t, w));
    return inserted_it[1].second;
  }
};
}
