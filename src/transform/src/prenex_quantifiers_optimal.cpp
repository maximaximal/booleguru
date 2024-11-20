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

using namespace booleguru::expression;

namespace booleguru::transform {
struct prenex_quantifier_optimal::node {
  // quantifier may be None, Exists or Forall. The only place where it is a None
  // is the root of the Tree, which gets children of all quantifiers.
  std::vector<node_ptr> children;
  std::vector<op_id> vars;
  op_type quantifier = op_type::None;
  bool on_critical_path = false;
  uint32_t height = 0;
  uint32_t depth = 0;
  uint32_t f = std::numeric_limits<uint32_t>::max();

  node() = default;
  node(std::vector<node_ptr> children, op_type quantifier = op_type::None)
    : children(children)
    , quantifier(quantifier) {}
};

struct prenex_quantifier_optimal::inner {
  std::stack<node_ptr> s;
  std::vector<node_ptr> critical_path;
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

  // TODO: Better approach is to keep the "None" node in the tree and
  // remove it afterwards. The arithmetic-based approach still works
  // out then (or at least it should).
  
  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  assign_height_depth(*t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  preprocess(t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  assign_height_depth(*t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  extract_critical_path(t);

  root_quantifier_ = t->quantifier;

  f(t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  prenex(t);

  if(t)
    conditionally_create_animation_step(o.get_mgr(), t);

  expression::op_manager& mgr = o.get_mgr();
  for(auto& b : i->critical_path | std::views::reverse) {
    for(op_id id : b->vars) {
      new_root = mgr.get_id(expression::op(b->quantifier, id, new_root));
    }
  }

  return o.get_mgr()[new_root];
}

void
prenex_quantifier_optimal::preprocess(node_ptr& root) {
  for(auto& c : root->children) {
    fmt::println("Child Quant: {} And Depth: {}", c->quantifier, c->depth);
  }

  if(root->quantifier == expression::op_type::None) {
    assert(root->children.size() > 0);
    std::sort(
      root->children.begin(),
      root->children.end(),
      [](const auto& l, const auto& r) { return l->depth >= r->depth; });

    std::vector<node_ptr> highest_depths;
    highest_depths.push_back(root->children[0]);
    for(size_t i = 1; i < root->children.size(); ++i) {
      if(root->children[i]->depth == highest_depths[0]->depth) {
        highest_depths.push_back(root->children[i]);
      } else {
        break;
      }
    }

    auto it = std::find_if(
      highest_depths.begin(), highest_depths.end(), [this](const node_ptr& p) {
        if(d1_ == up) {
          return prioritized_ == p->quantifier;
        } else {
          return prioritized_ != p->quantifier;
        }
      });

    auto found = *it;

    auto root_child_it = std::find_if(
      root->children.begin(), root->children.end(), [found](const node_ptr& p) {
        return p.get() == found.get();
      });

    node_ptr new_root;
    if(root_child_it != root->children.end()) {
      new_root = *root_child_it;
      root->children.erase(root_child_it);
    } else {
      new_root = root->children[root->children.size() - 1];
      root->children.pop_back();
    }
    std::copy(root->children.begin(),
              root->children.end(),
              std::back_inserter(new_root->children));
    root = new_root;
  }

  assert(root);
  assert(root->quantifier != op_type::None);
  std::stack<node_ptr> s;
  s.emplace(root);

  while(!s.empty()) {
    node_ptr n = s.top();
    s.pop();

    if(n->children.size() == 1) {
      node_ptr c = n->children[0];
      n->children.clear();
      while(c->quantifier == n->quantifier) {
        std::copy(c->vars.begin(), c->vars.end(), std::back_inserter(n->vars));
        if(c->children.size() == 1) {
          c = c->children[0];
        } else {
          c = nullptr;
          for(auto c : c->children) {
            s.emplace(c);
          }
        }
      }
      if(c && c->quantifier != n->quantifier) {
        n->children.emplace_back(c);
      }
    } else {
      bool changing = true;
      while(changing) {
        changing = false;
        for(size_t i = 0; i < n->children.size(); ++i) {
          node_ptr c = n->children[i];

          // Multiple children could have the same quantifier type. In
          // this case, they must be merged with n.
          if(c->quantifier == n->quantifier) {
            std::copy(
              c->vars.begin(), c->vars.end(), std::back_inserter(n->vars));
            n->children.erase(n->children.begin() + i);
            std::copy(c->children.begin(),
                      c->children.end(),
                      std::back_inserter(n->children));
	    changing = true;
          }
        }
      }
      for(auto c : n->children) {
	s.emplace(c);
      }
    }
  }

  op_type first_quantifier = prioritized_;
  if(d1_ == down)
    first_quantifier = op_type_flip_quantifier(first_quantifier);

  if(root->quantifier != first_quantifier) {
    node_ptr new_root
      = std::make_shared<node>(std::vector<node_ptr>{ root }, first_quantifier);
    root = new_root;
  }
}

uint32_t
prenex_quantifier_optimal::assign_height_depth(node& n, uint32_t h) {
  n.height = h;
  uint32_t dp = 1;
  for(auto& c : n.children) {
    uint32_t dp_c
      = assign_height_depth(*c, c->quantifier != n.quantifier ? h + 1 : h);
    if(c->quantifier != n.quantifier) {
      ++dp_c;
    }
    dp = std::max(dp_c, dp);
  }
  n.depth = dp;
  return dp;
}

void
prenex_quantifier_optimal::extract_critical_path(const node_ptr& root) {
  node_ptr n = root;
  size_t idx = 0;
  i->critical_path.resize(root->depth);

  fmt::println("Critical Path Size: {}", i->critical_path.size());
  while(n) {
    assert(idx < i->critical_path.size());
    i->critical_path[idx++] = n;
    n->on_critical_path = true;

    for(auto& c : n->children) {
      fmt::println(
        "IDX {}, Child: Q {} D {}", idx - 1, c->quantifier, c->depth);
    }

    std::vector<node_ptr> filtered;
    std::copy_if(
      n->children.begin(),
      n->children.end(),
      std::back_inserter(filtered),
      [&n](const node_ptr& e) { return e->quantifier != n->quantifier; });

    auto it = std::max_element(
      filtered.begin(),
      filtered.end(),
      [&n, idx](const node_ptr& a, const node_ptr& b) {
        // This check is required, as there may be
        // multiple children with the same depths but
        // different quantifiers. Otherwise, wrong
        // paths may be chosen!

        fmt::println("IDX: {}, NQ: {}, Ad: {}, AQ: {}, Bd: {}, BQ: {}",
                     idx - 1,
                     n->quantifier,
                     a->depth,
                     a->quantifier,
                     b->depth,
                     b->quantifier);

        return a->depth < b->depth;
      });
    if(it != filtered.end()) {
      n = *it;
    } else {
      n = nullptr;
    }
  }
}

uint32_t
prenex_quantifier_optimal::f(node_ptr n, node_ptr parent) {
  assert(n->quantifier != op_type::None);
  assert(n->quantifier == op_type::Exists || n->quantifier == op_type::Forall);

  uint32_t x;
  if(n->quantifier == prioritized_) {
    x = f_1(n, parent);
  } else {
    x = f_2(n, parent);
  }
  return x;
}

uint32_t
prenex_quantifier_optimal::f_1(node_ptr n, node_ptr parent) {
  uint32_t x;
  if(d1_ == up) {
    x = f_1_up(n, parent);
  } else {
    x = f_1_down(n, parent);
  }

  n->f = x;

  for(auto c : n->children) {
    f(c, n);
  }
  return x;
}
uint32_t
prenex_quantifier_optimal::f_2(node_ptr n, node_ptr parent) {
  if(d2_ == up) {
    return f_2_up(n, parent);
  } else {
    return f_2_down(n, parent);
  }
}

uint32_t
prenex_quantifier_optimal::f_1_up(node_ptr n, node_ptr parent) {
  (void)parent;
  return n->height;
}
uint32_t
prenex_quantifier_optimal::f_1_down(node_ptr n, node_ptr parent) {
  (void)parent;
  static_assert(static_cast<uint32_t>(op_type::Exists != op_type::Forall) == 1);
  static_assert(static_cast<uint32_t>(op_type::Exists == op_type::Forall) == 0);

  uint32_t x = i->critical_path.size() - n->depth;
  uint32_t delta
    = (static_cast<uint32_t>(prioritized_ == root_quantifier_) + x + 1) % 2;
  return x - delta;
}

uint32_t
prenex_quantifier_optimal::f_2_up(node_ptr n, node_ptr parent) {
  n->f = [&n, &parent]() -> uint32_t {
    if(parent) {
      assert(parent->f != std::numeric_limits<uint32_t>::max());
      return (parent->f)
             + static_cast<uint32_t>(n->quantifier != parent->quantifier);
    } else {
      return 0;
    }
  }();

  for(auto c : n->children) {
    f(c, n);
  }
  return n->f;
}
uint32_t
prenex_quantifier_optimal::f_2_down(node_ptr n, node_ptr parent) {
  (void)parent;
  uint32_t l = i->critical_path.size() - 1;
  uint32_t delta
    = (static_cast<uint32_t>(prioritized_ == root_quantifier_) + l) % 2;
  uint32_t b = l - delta;

  for(auto c : n->children) {
    // n->f is still unassigned, but the f_2_up will never be called afterwards.
    // Guarded by an assertion in f_2_up.
    uint32_t x = f(c, n);
    uint32_t gamma = c->quantifier != n->quantifier;
    b = std::min(b, x - gamma);
  }

  n->f = b;
  return b;
}

void
prenex_quantifier_optimal::prenex(node_ptr root) {
  (void)root;
  (void)kind_;
  std::stack<node_ptr> s;
  s.emplace(root);

  while(!s.empty()) {
    node_ptr p = s.top();
    s.pop();
    for(auto c : p->children) {
      assert(c);
      s.emplace(c);
    }

    if(!p->on_critical_path) {
      fmt::println("f: {}, crit: {}", p->f, i->critical_path.size());
      assert(p->f < i->critical_path.size());
      auto& src = p->vars;
      assert(i->critical_path[p->f]);
      auto& tgt = i->critical_path[p->f]->vars;
      std::copy(src.begin(), src.end(), std::back_inserter(tgt));
    }
  }

  for(auto& c : i->critical_path) {
    c->children.erase(std::remove_if(c->children.begin(),
                                     c->children.end(),
                                     [](auto& e) {
                                       assert(e);
                                       return !e->on_critical_path;
                                     }),
                      c->children.end());
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
      top ? std::vector<node_ptr>{ top } : std::vector<node_ptr>({}), q);
  }
  n->vars.emplace_back(var);
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
        l->children.emplace_back(c);
      }
      i->s.emplace(l);
    } else if(l->quantifier == op_type::None
              && r->quantifier != op_type::None) {
      l->children.emplace_back(r);
      i->s.emplace(l);
    } else if(l->quantifier != op_type::None
              && r->quantifier == op_type::None) {
      r->children.emplace_back(l);
      i->s.emplace(r);
    } else if(l->quantifier != op_type::None
              && r->quantifier != op_type::None) {
      node_ptr n = std::make_shared<node>(std::vector<node_ptr>({ l, r }),
                                          op_type::None);
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
    const char* type = "N";
    if(p->quantifier == op_type::Forall)
      type = "∀";
    if(p->quantifier == op_type::Exists)
      type = "∃";

    o << "  " << id << " [ shape=\"box\",";
    if(p->on_critical_path)
      o << " color=\"red\", ";
    o << " label=\"";
    if(p->on_critical_path)
      o << fmt::format("{} (ht:{},dp:{},idx:{}):\n{}",
                       type,
                       p->height,
                       p->depth,
                       p->height - 1,
                       fmt::join(v, ", "));
    else
      o << fmt::format("{} (ht:{},dp:{},f:{}):\n{}",
                       type,
                       p->height,
                       p->depth,
                       p->f,
                       fmt::join(v, ", "));
    o << "\" ];\n";

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
