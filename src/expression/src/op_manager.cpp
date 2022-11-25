#include <cassert>
#include <ostream>
#include <stack>

#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

namespace booleguru::expression {
inline constexpr static bool
parens_required(op_type p, op_type c) {
  return static_cast<size_t>(p) > static_cast<size_t>(c);
}

template<typename T>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const T& expr,
               op_type parent_type = op_type::None) {
  (void)o;
  (void)exprs;
  (void)t;
  (void)expr;
  (void)parent_type;
  assert(false);
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::un)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::bin)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::var)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::quant)& op,
               op_type parent_type);

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::bin)& op,
               op_type parent_type) {
  bool parens = parens_required(parent_type, t);
  if(parens)
    o << "(";
  exprs[op.l]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  o << " " << op_type_to_sym(t) << " ";
  exprs[op.r]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  if(parens)
    o << ")";
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::quant)& expr,
               op_type parent_type) {
  (void)parent_type;
  auto varop = exprs[expr.v];
  assert(expr.v > 0);

  o << op_type_to_sym(t) << exprs.vars()[varop->var.v]->name << " ";
  exprs[expr.e]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  return o;
}

op_ref
op_ref::left() {
  if(!valid())
    return op_ref();
  return get_mgr()[(*this)->left()];
}

op_ref
op_ref::right() {
  if(!valid())
    return op_ref();
  return get_mgr()[(*this)->right()];
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::un)& expr,
               op_type parent_type) {
  (void)parent_type;
  o << op_type_to_sym(t);
  exprs[expr.c]->visit([&o, &exprs, t](op_type ct, auto& e) {
    op_tostr_visit(o, exprs, ct, e, t);
  });
  return o;
}

template<>
inline std::ostream&
op_tostr_visit(std::ostream& o,
               const op_manager& exprs,
               op_type t,
               const decltype(op::var)& expr,
               op_type parent_type) {
  (void)t;
  (void)parent_type;
  return o << exprs.vars()[expr.v]->name;
}

std::ostream&
operator<<(std::ostream& o, const op_ref& e) {
  if(!e.valid())
    return o << "(-)";
  e->visit([&o, &e](op_type t, const auto& ex) {
    op_tostr_visit(o, e.get_mgr(), t, ex);
  });
  return o;
}

op_manager::op_manager()
  : vars_(std::make_shared<var_manager>()) {}

op_manager::op_manager(std::shared_ptr<var_manager> vars)
  : vars_(vars) {}

op_manager::base::objref
op_manager::insert(T&& obj, size_t obj_hash) {
  switch(obj.type) {
    case op_type::And:
      // And And should just keep the and it already has.
      break;
    case op_type::Or:
    case op_type::Lpmi:
    case op_type::Impl:
    case op_type::Equi:
      obj.and_inside =
        objects_[obj.left()].and_inside || objects_[obj.right()].and_inside;
      break;
    case op_type::Not:
      obj.and_inside = objects_[obj.left()].and_inside;
      break;
    case op_type::Exists:
    case op_type::Forall:
      obj.and_inside = objects_[obj.left()].and_inside;
      break;
    case op_type::Var:
      assert(obj.var.v < vars().size());
      break;
    default:
      break;
  }

  return base::insert(std::move(obj), obj_hash);
}
void
op_manager::modify_ops(modifier&& mod) {
  for(auto& op : objects_) {
    mod(op);
  }
}

void
op_manager::unmark() {
  for(auto& op : objects_) {
    op.mark = false;
  }
}

void
op_manager::reset_op_user_vars() {
  for(auto& op : objects_) {
    op.user_flag3 = false;
    op.user_flag4 = false;
    op.user_flag5 = false;
    op.user_flag6 = false;
    op.user_flag7 = false;
    op.user_flag8 = false;
    op.user_int16 = 0;
    op.user_int32 = 0;
  }
}

void
op_manager::mark_through_tree(uint32_t root) {
  std::stack<uint32_t> unvisited;
  unvisited.push(root);

  while(!unvisited.empty()) {
    op& current = objects_[unvisited.top()];
    unvisited.pop();

    if(current.mark)
      continue;

    current.mark = true;

    switch(current.type) {
      case op_type::Exists:
      case op_type::Forall:
        unvisited.push(current.quant.e);
        break;
      case op_type::Not:
        unvisited.push(current.un.c);
        break;
      case op_type::And:
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        unvisited.push(current.bin.l);
        unvisited.push(current.bin.r);
        break;
      case op_type::Var:
      case op_type::None:
        break;
    }
  }
}
void
op_manager::traverse_depth_first_through_tree(
  uint32_t root,
  std::function<void(uint32_t, op&)>& visit) {
  std::stack<uint32_t> unvisited;
  unvisited.push(root);

  while(!unvisited.empty()) {
    uint32_t id = unvisited.top();
    op& current = objects_[id];
    unvisited.pop();

    visit(id, current);

    switch(current.type) {
      case op_type::Exists:
      case op_type::Forall:
        unvisited.push(current.quant.e);
        break;
      case op_type::Not:
        unvisited.push(current.un.c);
        break;
      case op_type::And:
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        unvisited.push(current.bin.l);
        unvisited.push(current.bin.r);
        break;
      case op_type::Var:
      case op_type::None:
        break;
    }
  }
}
void
op_manager::traverse_unmarked_depth_first_through_tree(
  uint32_t root,
  std::function<void(uint32_t, op&)> visit) {
  std::stack<uint32_t> unvisited;
  unvisited.push(root);

  while(!unvisited.empty()) {
    uint32_t id = unvisited.top();
    op& current = objects_[id];
    unvisited.pop();

    if(current.mark)
      continue;

    visit(id, current);

    switch(current.type) {
      case op_type::Exists:
      case op_type::Forall:
        unvisited.push(current.quant.e);
        break;
      case op_type::Not:
        unvisited.push(current.un.c);
        break;
      case op_type::And:
      case op_type::Or:
      case op_type::Equi:
      case op_type::Impl:
      case op_type::Lpmi:
      case op_type::Xor:
        unvisited.push(current.bin.l);
        unvisited.push(current.bin.r);
        break;
      case op_type::Var:
      case op_type::None:
        break;
    }
  }
}
}
