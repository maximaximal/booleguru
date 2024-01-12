#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/quanttree.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/transform/prenex_quantifiers_optimal.hpp>
#include <booleguru/util/reverse.hpp>
#include <booleguru/util/unsupported.hpp>

#include <algorithm>
#include <fstream>
#include <iosfwd>
#include <iterator>
#include <limits>
#include <ranges>
#include <set>
#include <stack>
#include <unordered_map>

#include <fmt/format.h>
#include <fmt/ranges.h>

using namespace booleguru::expression;

namespace booleguru::transform {
struct prenex_quantifier_optimal::node {
  // quantifier may be None, Exists or Forall. The only place where it is a None
  // is the root of the Tree, which gets children of all quantifiers.
  std::set<node_ptr> children;
  std::set<op_id> vars;
  op_type quantifier = op_type::None;

  node() = default;
  node(std::set<node_ptr> children, op_type quantifier = op_type::None)
    : children(children)
    , quantifier(quantifier) {}
};

struct prenex_quantifier_optimal::inner {
  std::stack<node_ptr> s;
};

prenex_quantifier_optimal::prenex_quantifier_optimal(kind k)
  : i(std::make_unique<inner>())
  , kind_(k) {}
prenex_quantifier_optimal::~prenex_quantifier_optimal() {}

op_ref
prenex_quantifier_optimal::operator()(expression::op_ref o) {
  op_id new_root = o.get_mgr().traverse_postorder_with_stack(
    o.get_id(), [this](op_manager* ops, op_id o) -> op_id {
      return walk((*ops)[o]).get_id();
    });

  if(!encountered_quant_)
    return o;

  node_ptr t = i->s.top();
  i->s.pop();

  assert(i->s.empty());

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  preprocess(t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  prenex(t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  return o.get_mgr()[new_root];
}

void
prenex_quantifier_optimal::preprocess(node_ptr root) {
  assert(root);
  std::stack<node_ptr> s;
  s.emplace(root);

  while(!s.empty()) {
    node_ptr n = s.top();
    s.pop();

    bool changing = false;
    do {
      changing = false;
      for(auto it = n->children.begin(); it != n->children.end(); ++it) {
        node_ptr c = *it;

        // Merge nodes if they have the same quantifier! Only alternating sets
        // are allowed.
        if(n->quantifier == c->quantifier) {
          for(auto cc : c->children) {
            n->children.emplace(cc);
          }
          for(auto v : c->vars) {
            n->vars.emplace(v);
          }

          changing = true;

          n->children.erase(it);
          break;
        }
      }
    } while(changing);

    for(auto c : n->children) {
      s.emplace(c);
    }
  }
}

void
prenex_quantifier_optimal::prenex(node_ptr root) {
  (void)root;
  switch(kind_) {
    case Eup_up:
    case Eup_down:
    case Edown_up:
    case Edown_down:
    case Aup_up:
    case Aup_down:
    case Adown_up:
    case Adown_down:
      break;
  }
}

op_ref
prenex_quantifier_optimal::walk(expression::op_ref o) {
  using enum expression::op_type;
  switch(o->type) {
    case Forall:
      [[fallthrough]];
    case Exists:
      return walk_quant(o);
    case Var:
      i->s.emplace(nullptr);
      return o;
    case Not:
      return walk_not(o);
    case Impl:
      return walk_impl(o);
    case Lpmi:
      return walk_lpmi(o);
    case Equi:
      return walk_equi(o);
    case Or:
      [[fallthrough]];
    case And:
      return walk_bin(o);
    case Xor:
      return walk_xor(o);
    default:
      return o;
  }
}

op_ref
prenex_quantifier_optimal::rebind_variable(expression::op_ref o,
                                           expression::op_ref bound_v) {
  assert(o->is_quant());
  op_ref e = o.right();
  op_id new_e = o.get_mgr().traverse_postorder_with_stack(
    e.get_id(), [bound_v](op_manager* ops, op_id id) -> op_id {
      op_ref o = (*ops)[id];
      switch(o->type) {
        case op_type::Var:
          if(o->var.v == bound_v->var.v && !o->var.q) {
            // Replace the unbound variable with the bound variant.
            return bound_v.get_id();
          }
          return id;
        default:
          return id;
      }
    });
  return o.get_mgr()[new_e];
}

op_ref
prenex_quantifier_optimal::walk_quant(expression::op_ref o) {
  encountered_quant_ = true;

  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);
  uint32_t bound = old_v_obj.counter++;
  op_type q = o->type;

  op_id var = 0;

  if(bound > 0) {
    auto bound_v = o.get_mgr().get(
      expression::op(expression::op_type::Var, old_v.v, bound, old_v.i));
    var = bound_v.get_id();
    o = rebind_variable(o, bound_v);
  } else {
    var = o.left().get_id();
    o = o.right();
  }

  // All occurrences of this variable are now rebound, the var_id is globally
  // unique.
  node_ptr n;

  node_ptr top = i->s.top();
  i->s.pop();
  // Pop the variable! This is always a nullptr and was inserted by the
  // traversal of a op_type::Var (o.left()) node earlier.
  assert(!i->s.top());
  i->s.pop();
  if(top && top->quantifier == q)
    n = top;
  else if(top && top->quantifier == op_type::None) {
    n = top;
    n->quantifier = q;
  } else {
    n = std::make_shared<node>(
      top ? std::set<node_ptr>{ top } : std::set<node_ptr>({}), q);
  }
  n->vars.emplace(var);
  i->s.emplace(n);

  // Remove the quantifier, i.e. only return the right sub-tree.
  return o;
}

op_ref
prenex_quantifier_optimal::walk_not(expression::op_ref o) {
  // All quantifiers downwards have to be inverted!

  std::stack<node_ptr> s;
  s.emplace(i->s.top());

  while(!s.empty()) {
    node_ptr n = s.top();
    s.pop();

    if(!n)
      continue;

    n->quantifier = op_type_flip_quantifier(n->quantifier);

    for(auto& child : n->children) {
      if(child)
        s.emplace(child);
    }
  }

  return o;
}

void
prenex_quantifier_optimal::emplace_l_r(node_ptr& l, node_ptr& r) {
  if(!l && !r)
    i->s.emplace(nullptr);
  else if(!l && r)
    i->s.emplace(r);
  else if(l && !r)
    i->s.emplace(l);
  else if(l && r) {
    if(l->quantifier == op_type::None && r->quantifier == op_type::None) {
      for(const auto& c : r->children) {
        if(!c)
          continue;
        l->children.emplace(c);
      }
      i->s.emplace(l);
    } else if(l->quantifier == op_type::None
              && r->quantifier != op_type::None) {
      l->children.emplace(r);
      i->s.emplace(l);
    } else if(l->quantifier != op_type::None
              && r->quantifier == op_type::None) {
      r->children.emplace(l);
      i->s.emplace(r);
    } else if(l->quantifier != op_type::None
              && r->quantifier != op_type::None) {
      node_ptr n
        = std::make_shared<node>(std::set<node_ptr>({ l, r }), op_type::None);
      i->s.emplace(n);
    } else {
      assert(false);
    }
  }
}

op_ref
prenex_quantifier_optimal::walk_impl(expression::op_ref o) {
  auto r = i->s.top();
  i->s.pop();
  auto l = i->s.top();
  i->s.pop();

  // Invert left side by calling the walk_not function. walk_not does not change
  // the the op_ref, it only operates on i->s.
  i->s.emplace(l);
  walk_not(o.left());
  l = i->s.top();
  i->s.pop();

  // Now we are done. Add the two sub-trees from i->s to children of a new node,
  // if they exist.
  emplace_l_r(l, r);
  return o;
}

op_ref
prenex_quantifier_optimal::walk_lpmi(expression::op_ref o) {
  auto r = i->s.top();
  i->s.pop();
  auto l = i->s.top();
  i->s.pop();

  // Invert right side by calling the walk_not function. walk_not does not
  // change the the op_ref, it only operates on i->s.
  i->s.emplace(r);
  walk_not(o.right());
  r = i->s.top();
  i->s.pop();

  // Now we are done. Add the two sub-trees from i->s to children of a new node,
  // if they exist.
  emplace_l_r(l, r);
  return o;
}

op_ref
prenex_quantifier_optimal::walk_bin(expression::op_ref o) {
  auto r = i->s.top();
  i->s.pop();
  auto l = i->s.top();
  i->s.pop();

  emplace_l_r(l, r);
  return o;
}

op_ref
prenex_quantifier_optimal::walk_equi(expression::op_ref o) {
  (void)o;
  throw util::unsupported("equi unsupported for optimal prenexing");
}

op_ref
prenex_quantifier_optimal::walk_xor(expression::op_ref o) {
  (void)o;
  throw util::unsupported("xor unsupported for optimal prenexing");
}

void
prenex_quantifier_optimal::conditionally_create_animation_step(
  op_manager& mgr,
  const node_ptr& root) {
  if(!animate_)
    return;

  std::string name = "step" + std::to_string(animation_step_);
  ++animation_step_;
  std::ofstream f(animation_path_ + "_" + name + ".dot");
  to_dot(mgr, root, f);
}

void
prenex_quantifier_optimal::to_dot(op_manager& mgr,
                                  const node_ptr& root,
                                  std::ostream& o) {
  assert(root);

  o << "digraph {\n";

  std::stack<node_ptr> s;
  s.emplace(root);

  std::unordered_map<node*, int> ids;
  int id_counter = 0;

  auto get_id = [&ids, &id_counter](const node_ptr& n) {
    int id = 0;
    auto it = ids.find(n.get());
    if(it == ids.end()) {
      id = id_counter;
      ids.insert(std::make_pair(n.get(), id_counter++));
    } else {
      id = it->second;
    }
    return id;
  };

  // Nullptr should be ID 0 for easier debugging.
  get_id(nullptr);

  while(!s.empty()) {
    const node_ptr p = s.top();
    s.pop();
    if(!p)
      continue;

    auto v = std::views::transform(p->vars, [mgr](const auto& e) {
      return fmt::format("{}", mgr[e].to_string());
    });

    int id = get_id(p);
    char type = 'N';
    if(p->quantifier == op_type::Forall)
      type = 'A';
    if(p->quantifier == op_type::Exists)
      type = 'E';
    o << "  " << id << " [ label=\""
      << fmt::format("{}: {}", type, fmt::join(v, ", ")) << "\" ];\n";

    for(const auto& c : p->children) {
      if(!c)
        continue;
      o << "  " << id << " -> " << get_id(c) << ";\n";
      s.emplace(c);
    }
  }

  o << "}\n";
}
}
