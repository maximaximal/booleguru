#pragma once

#include <unordered_map>

#include "visitor.hpp"

#include <booleguru/expression/var_manager.hpp>

namespace booleguru::transform {
namespace actors {
template<class I>
struct variable_rename : public I {
  using M =
    std::unordered_map<expression::op_ref::ref, expression::op_ref::ref>;
  using ret = typename I::ret;
  using I::I;

  M* vars = nullptr;

  inline ret walk_var(expression::op_ref e) {
    if constexpr(std::is_same_v<ret, visitor_descent_query>) {
      return I::vd(e);
    } else {
      assert(vars);
      auto it = vars->find(e->var.v);
      if(it != vars->end())
        return e.get_mgr().get(
          expression::op(expression::op_type::Var, it->second, 0));
      else
        return e;
    }
  }
};
}
struct variable_rename
  : public visitor<variable_rename, actors::variable_rename> {
  using M =
    std::unordered_map<expression::op_ref::ref, expression::op_ref::ref>;
  M vars;

  template<typename... Rest>
  variable_rename(expression::var_manager& varmgr,
                  const std::string& oldname,
                  const std::string& newname,
                  Rest&&... rest) {
    collect_.vars = &vars;
    traverse_.vars = &vars;

    vars.insert({ varmgr.get(expression::variable{ oldname }).get_id(),
                  varmgr.get(expression::variable{ newname }).get_id() });
    if constexpr(sizeof...(Rest) > 0) {
      variable_rename(varmgr, std::forward(rest)...);
    }
  }

  variable_rename(expression::var_manager& varmgr,
                  const std::unordered_map<std::string, std::string>& map);
};
}
