#pragma once

#include <memory>

#include "op_manager.hpp"
#include "var_manager.hpp"

namespace booleguru::expression::literals {
class handle {
  static std::unique_ptr<handle> instance_;

  std::shared_ptr<op_manager> ops_ = std::make_shared<op_manager>();

  public:
  static handle& global() {
    if(!instance_)
      instance_ = std::make_unique<handle>();
    return *instance_;
  }

  /* Delete the global instance.
   */
  static void reset_global() { instance_.reset(); }

  op_manager& get_op_manager() { return *ops_; }
  var_manager& get_var_manager() { return ops_->vars(); }
};

template<class Mgr>
class proxy {
  std::string_view name_;

  using Ref = typename Mgr::objref;

  public:
  constexpr inline proxy(std::string_view name)
    : name_(name) {}

  constexpr inline Ref operator()(Mgr& mgr) const {
    if constexpr(std::is_same<Mgr, var_manager>()) {
      return mgr.get(variable{ std::string(name_) });
    } else if constexpr(std::is_same<Mgr, op_manager>()) {
      return mgr.get(
        op{ op_type::Var,
            mgr.vars().get(variable{ std::string(name_) }).get_id(),
            0 });
    }
  }
  inline Ref operator()(std::shared_ptr<Mgr> mgr) const {
    return (*this)(*mgr);
  }

  constexpr operator Ref() const {
    if constexpr(std::is_same<Mgr, var_manager>()) {
      return (*this)(handle::global().get_var_manager());
    } else if constexpr(std::is_same<Mgr, op_manager>()) {
      return (*this)(handle::global().get_op_manager());
    }
  }
};

[[nodiscard]] constexpr inline proxy<var_manager> operator"" _varref(
  const char* name,
  std::size_t l) {
  return proxy<var_manager>(std::string_view(name, l));
}

[[nodiscard]] constexpr inline proxy<op_manager> operator"" _var(
  const char* name,
  std::size_t l) {
  return proxy<op_manager>(std::string_view(name, l));
}

[[nodiscard]] constexpr inline op_ref
exists(op_ref variable, op_ref sub_tree) {
  if(!std::is_constant_evaluated()) {
    assert(variable.valid());
    assert(sub_tree.valid());
    assert(&variable.get_mgr() == &sub_tree.get_mgr());
  }
  return sub_tree.get_mgr().get(
    op(op_type::Exists, variable.get_id(), sub_tree.get_id()));
}

[[nodiscard]] constexpr inline op_ref
exists(var_ref variable, op_ref sub_tree) {
  if(!std::is_constant_evaluated()) {
    assert(&variable.get_mgr() == &sub_tree.get_mgr().vars());
  }
  op_ref var_op =
    sub_tree.get_mgr().get(op(op_type::Var, variable.get_id(), 0));
  return exists(var_op, sub_tree);
}

[[nodiscard]] constexpr inline op_ref
forall(op_ref variable, op_ref sub_tree) {
  if(!std::is_constant_evaluated()) {
    assert(variable.valid());
    assert(sub_tree.valid());
    assert(&variable.get_mgr() == &sub_tree.get_mgr());
  }
  return sub_tree.get_mgr().get(
    op(op_type::Forall, variable.get_id(), sub_tree.get_id()));
}

[[nodiscard]] constexpr inline op_ref
forall(var_ref variable, op_ref sub_tree) {
  if(!std::is_constant_evaluated()) {
    assert(&variable.get_mgr() == &sub_tree.get_mgr().vars());
  }
  op_ref var_op =
    sub_tree.get_mgr().get(op(op_type::Var, variable.get_id(), 0));
  return forall(var_op, sub_tree);
}

[[nodiscard]] constexpr inline op_ref
lpmi(op_ref left, op_ref right) {
  if(!std::is_constant_evaluated()) {
    assert(&left.get_mgr() == &left.get_mgr());
  }
  return left.get_mgr().get(op(op_type::Lpmi, left.get_id(), right.get_id()));
}

[[nodiscard]] constexpr inline op_ref
impl(op_ref left, op_ref right) {
  if(!std::is_constant_evaluated()) {
    assert(&left.get_mgr() == &left.get_mgr());
  }
  return left.get_mgr().get(op(op_type::Impl, left.get_id(), right.get_id()));
}

[[nodiscard]] constexpr inline op_ref
equi(op_ref left, op_ref right) {
  if(!std::is_constant_evaluated()) {
    assert(&left.get_mgr() == &left.get_mgr());
  }
  return left.get_mgr().get(op(op_type::Equi, left.get_id(), right.get_id()));
}
}
