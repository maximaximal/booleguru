#pragma once

#include <unordered_map>

#include "visitor.hpp"

#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
struct variable_rename : public visitor<variable_rename> {
  using M
    = std::unordered_map<op_ref::ref::numeric_type, op_ref::ref::numeric_type>;
  M vars;

  template<typename... Rest>
  variable_rename(expression::var_manager& varmgr,
                  const std::string& oldname,
                  const std::string& newname,
                  Rest&&... rest) {
    vars.insert({ varmgr.get(variable{ oldname }).get_id(),
                  varmgr.get(variable{ newname }).get_id() });
    if constexpr(sizeof...(Rest) > 0) {
      variable_rename(varmgr, std::forward(rest)...);
    }
  }

  variable_rename(expression::var_manager& varmgr,
                  const std::unordered_map<std::string, std::string>& map);

  op_ref walk_var(op_ref e);
};
}
