#pragma once

#include <optional>

#include "visitor.hpp"

namespace booleguru::transform {
namespace actors {
template<class I>
struct variable_extend : public I {
  using ret = typename I::ret;
  using I::I;

  const std::string* prefix = nullptr;
  const std::string* suffix = nullptr;

  inline ret walk_var(expression::op_ref e) {
    if constexpr(std::is_same_v<ret, visitor_descent_query>) {
      return I::vd(e);
    } else {
      auto& oldvar = e.get_mgr().vars().getobj(e->var.v);
      auto newvar = e.get_mgr().vars().get(
        expression::variable{ *prefix + oldvar.name + *suffix });
      return e.get_mgr().get(
        expression::op(expression::op_type::Var, newvar.get_id(), 0));
    }
  }
};
}
struct variable_extend
  : public visitor<variable_extend, actors::variable_extend> {
  using base = visitor<variable_extend, actors::variable_extend>;
  inline variable_extend(const std::string& prefix, const std::string& suffix)
    : prefix(prefix)
    , suffix(suffix) {
    collect_.prefix = &this->prefix;
    collect_.suffix = &this->suffix;
    traverse_.prefix = &this->prefix;
    traverse_.suffix = &this->suffix;
  }

  std::string prefix;
  std::string suffix;
};

expression::op_ref
operator+(expression::op_ref& e, const std::string& s);

expression::op_ref
operator+(expression::op_ref& e, int n);
}
