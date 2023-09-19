#include <algorithm>

#include <booleguru/transform/variable_rename.hpp>

namespace booleguru::transform {
variable_rename::variable_rename(
  expression::var_manager& varmgr,
  const std::unordered_map<std::string, std::string>& map) {
  vars.reserve(map.size());

  std::transform(
    map.begin(),
    map.end(),
    std::inserter(vars, vars.end()),
    [&varmgr](auto& e) -> M::value_type {
      return M::value_type{ varmgr.get(variable{ e.first }).get_id(),
                            varmgr.get(variable{ e.second }).get_id() };
    });
}

expression::op_ref
variable_rename::walk_var(op_ref e) {
  auto it = vars.find(static_cast<uint32_t>(e->var.v));
  if(it != vars.end())
    return e.get_mgr().get(op(op_type::Var, it->second, 0, 0));
  else
    return e;
}
}
