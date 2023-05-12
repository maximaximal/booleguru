#include <booleguru/transform/prenex_quantifiers.hpp>

#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/quantvec.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <iostream>
#include <iterator>
#include <limits>

using std::cout;
using std::endl;

namespace booleguru::transform {

#define IT(I) ops[I->var] << ":" << *I

struct prenex_strategy {
  expression::quantvec& critical_path;
  expression::quantvec& remaining;
  expression::op_manager& ops;
}

struct prenex_quantifier_Eup_Aup : public prenex_strategy {
  [[nodiscard]] inline expression::quantvec& operator()() {
    remaining.reverse();

    auto cit = critical_path.begin();
    prenex_quantifier_stack_entry* ce = &*cit;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      ++would_be_next_cit;

      while(true) {
        // std::cout << "Now at " << ops[cit->var] << ":" << *cit << std::endl;
        int32_t nesting = std::numeric_limits<int32_t>::max();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting > nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting <= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == ce->t && e.nesting >= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        // std::cout << "Found in remaining: " << ops[it->var] << ":" << *it
        //           << std::endl;

        auto next_cit = cit;
        ++next_cit;
        cit = critical_path.emplace(next_cit, *it);
        remaining.erase(it);
      }

      if(cit == critical_path.end()) {
        std::copy(remaining.begin(),
                  remaining.end(),
                  std::back_inserter(critical_path));
        remaining.clear();
      }
    }

    return critical_path;
  }
};

struct prenex_quantifier_Eup_Adown {
  std::list<prenex_quantifier_stack_entry>& critical_path;
  std::list<prenex_quantifier_stack_entry>& remaining;
  expression::op_manager& ops;

  inline constexpr prenex_quantifier_Eup_Adown(
    std::list<prenex_quantifier_stack_entry>& critical_path,
    std::list<prenex_quantifier_stack_entry>& remaining,
    expression::op_manager& ops) noexcept
    : critical_path(critical_path)
    , remaining(remaining)
    , ops(ops) {}

  [[nodiscard]] inline std::list<prenex_quantifier_stack_entry>& operator()() {
    remaining.reverse();

    prenex_quantifier_stack_entry::mark_leaves(remaining);

    auto cit = critical_path.begin();
    prenex_quantifier_stack_entry* ce = &*cit;

    std::list<prenex_quantifier_stack_entry> ignored_foralls;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      ++would_be_next_cit;

      while(true) {
        // cout << "Now at " << IT(cit) << endl;
        int32_t nesting = std::numeric_limits<int32_t>::max();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting > nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting <= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == ce->t && e.nesting >= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        // std::cout << "Found in remaining: " << IT(it) << std::endl;

        if(it->t == expression::op_type::Forall) {
          // The whole sub-tree, i.e. everything following this exists, must be
          // ignored!
          int32_t nesting = it->nesting;
          auto it_ = it;
          ++it_;
          // cout << "Remove " << IT(it) << endl;
          ignored_foralls.splice(ignored_foralls.end(), remaining, it);
          it = it_;
          while(it != remaining.end() && it->nesting > nesting) {
            // cout << "Remove " << IT(it) << endl;
            it_ = it;
            ++it_;
            ignored_foralls.splice(ignored_foralls.end(), remaining, it);
            it = it_;
          }
        } else {
          auto next_cit = cit;
          ++next_cit;
          cit = critical_path.emplace(next_cit, *it);
          remaining.erase(it);
        }
      }

      if(cit == critical_path.end()) {
        break;
      }
    }

    /*
    for(auto r = remaining.begin(); r != remaining.end(); ++r) {
      cout << "Remaining before Splice: " << IT(r) << endl;
    }
    */

    remaining.splice(remaining.end(), ignored_foralls);

    /*
    for(auto r = remaining.begin(); r != remaining.end(); ++r) {
      cout << "Remaining after Splice: " << IT(r) << endl;
    }
    */

    cit = critical_path.end();
    --cit;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      --would_be_next_cit;

      while(true) {
        int32_t nesting = std::numeric_limits<int32_t>::min();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting < nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting >= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == expression::op_type::Forall &&
                                e.t == ce->t && e.nesting <= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        auto next_cit = cit;
        critical_path.emplace(next_cit, *it);
        remaining.erase(it);
      }

      if(cit == critical_path.begin()) {
        break;
      }
    }

    if(!remaining.empty()) {
      std::copy_if(remaining.begin(),
                   remaining.end(),
                   std::back_inserter(critical_path),
                   prenex_quantifier_stack_entry::is_exists);

      auto pos_forall = std::find_if(critical_path.rbegin(),
                                     critical_path.rend(),
                                     prenex_quantifier_stack_entry::is_forall)
                          .base();
      if(pos_forall == critical_path.end()) {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::back_inserter(critical_path),
                     prenex_quantifier_stack_entry::is_forall);
      } else {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::inserter(critical_path, pos_forall),
                     prenex_quantifier_stack_entry::is_forall);
      }
    }

    return critical_path;
  }
};

struct prenex_quantifier_Edown_Aup {
  std::list<prenex_quantifier_stack_entry>& critical_path;
  std::list<prenex_quantifier_stack_entry>& remaining;
  expression::op_manager& ops;

  inline constexpr prenex_quantifier_Edown_Aup(
    std::list<prenex_quantifier_stack_entry>& critical_path,
    std::list<prenex_quantifier_stack_entry>& remaining,
    expression::op_manager& ops) noexcept
    : critical_path(critical_path)
    , remaining(remaining)
    , ops(ops) {}

  [[nodiscard]] inline std::list<prenex_quantifier_stack_entry>& operator()() {
    remaining.reverse();

    prenex_quantifier_stack_entry::mark_leaves(remaining);

    auto cit = critical_path.begin();
    prenex_quantifier_stack_entry* ce = &*cit;

    std::list<prenex_quantifier_stack_entry> ignored_exists;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      ++would_be_next_cit;

      while(true) {
        // cout << "Now at " << IT(cit) << endl;
        int32_t nesting = std::numeric_limits<int32_t>::max();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting > nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting <= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == ce->t && e.nesting >= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        // cout << "Found in remaining: " << IT(it) << endl;

        if(it->t == expression::op_type::Exists) {
          // The whole sub-tree, i.e. everything following this exists, must be
          // ignored!
          int32_t nesting = it->nesting;
          auto it_ = it;
          ++it_;
          // cout << "Remove " << IT(it) << endl;
          ignored_exists.splice(ignored_exists.end(), remaining, it);
          it = it_;
          while(it != remaining.end() && it->nesting > nesting) {
            // cout << "Remove " << IT(it) << endl;
            it_ = it;
            ++it_;
            ignored_exists.splice(ignored_exists.end(), remaining, it);
            it = it_;
          }
        } else {
          auto next_cit = cit;
          ++next_cit;
          cit = critical_path.emplace(next_cit, *it);
          remaining.erase(it);
        }
      }

      if(cit == critical_path.end()) {
        break;
      }
    }

    // for(auto r = remaining.begin(); r != remaining.end(); ++r) {
    //   cout << "Remaining before Splice: " << IT(r) << endl;
    // }

    remaining.splice(remaining.end(), ignored_exists);

    // for(auto r = remaining.begin(); r != remaining.end(); ++r) {
    //   cout << "Remaining after Splice: " << IT(r) << endl;
    // }

    cit = critical_path.end();
    --cit;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      --would_be_next_cit;

      while(true) {
        int32_t nesting = std::numeric_limits<int32_t>::min();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting < nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting >= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == expression::op_type::Exists &&
                                e.t == ce->t && e.nesting <= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        // for(auto r = critical_path.begin(); r != critical_path.end(); ++r) {
        //   cout << "Critical Path: " << IT(r) << endl;
        // }

        // cout << "CE: " << IT(ce) << " and CIT: " << IT(cit) << " Found: " <<
        // IT(it) << endl;

        auto next_cit = cit;
        critical_path.emplace(next_cit, *it);
        remaining.erase(it);
      }

      if(cit == critical_path.begin()) {
        break;
      }
    }

    if(!remaining.empty()) {
      std::copy_if(remaining.begin(),
                   remaining.end(),
                   std::back_inserter(critical_path),
                   prenex_quantifier_stack_entry::is_forall);

      auto pos_exists = std::find_if(critical_path.rbegin(),
                                     critical_path.rend(),
                                     prenex_quantifier_stack_entry::is_exists)
                          .base();
      if(pos_exists == critical_path.end()) {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::back_inserter(critical_path),
                     prenex_quantifier_stack_entry::is_exists);
      } else {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::inserter(critical_path, pos_exists),
                     prenex_quantifier_stack_entry::is_exists);
      }
    }

    return critical_path;
  }
};

struct prenex_quantifier_Edown_Adown {
  std::list<prenex_quantifier_stack_entry>& critical_path;
  std::list<prenex_quantifier_stack_entry>& remaining;
  expression::op_manager& ops;

  inline constexpr prenex_quantifier_Edown_Adown(
    std::list<prenex_quantifier_stack_entry>& critical_path,
    std::list<prenex_quantifier_stack_entry>& remaining,
    expression::op_manager& ops) noexcept
    : critical_path(critical_path)
    , remaining(remaining)
    , ops(ops) {}

  [[nodiscard]] inline std::list<prenex_quantifier_stack_entry>& operator()() {
    critical_path.reverse();

    auto cit = critical_path.begin();
    prenex_quantifier_stack_entry* ce = &*cit;

    while(!remaining.empty()) {
      ce = &*cit;

      auto would_be_next_cit = cit;
      ++would_be_next_cit;

      while(true) {
        // std::cout << "Now at " << ops[cit->var] << ":" << *cit << std::endl;
        int32_t nesting = std::numeric_limits<int32_t>::min();
        auto it =
          std::find_if(remaining.begin(),
                       remaining.end(),
                       [ce, &nesting](const prenex_quantifier_stack_entry& e) {
                         if(e.nesting < nesting) {
                           // Smaller nesting means higher up in quantifier
                           // tree.
                           return false;
                         } else if(e.nesting >= nesting) {
                           nesting = e.nesting;
                         }
                         return e.t == ce->t && e.nesting <= ce->nesting;
                       });

        if(it == remaining.end()) {
          cit = would_be_next_cit;
          break;
        }

        // std::cout << "Found in remaining: " << ops[it->var] << ":" << *it
        //           << std::endl;

        auto next_cit = cit;
        ++next_cit;
        cit = critical_path.emplace(next_cit, *it);
        remaining.erase(it);
      }

      if(cit == critical_path.end()) {
        break;
      }
    }

    critical_path.reverse();

    if(!remaining.empty()) {
      // Exists first
      auto pos_exists = std::find_if(critical_path.rbegin(),
                                     critical_path.rend(),
                                     prenex_quantifier_stack_entry::is_exists)
                          .base();
      if(pos_exists == critical_path.end()) {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::back_inserter(critical_path),
                     prenex_quantifier_stack_entry::is_exists);
      } else {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::inserter(critical_path, pos_exists),
                     prenex_quantifier_stack_entry::is_exists);
      }

      auto pos_forall = std::find_if(critical_path.rbegin(),
                                     critical_path.rend(),
                                     prenex_quantifier_stack_entry::is_forall)
                          .base();
      if(pos_forall == critical_path.end()) {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::back_inserter(critical_path),
                     prenex_quantifier_stack_entry::is_forall);
      } else {
        std::copy_if(remaining.begin(),
                     remaining.end(),
                     std::inserter(critical_path, pos_forall),
                     prenex_quantifier_stack_entry::is_forall);
      }
      remaining.clear();
    }

    return critical_path;
  }
};

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::post_action(expression::op_ref o) {
  prenex_quantifier_quant_stack_t critical_path;

  // Critical Path is saved in reverse, as this makes analysis easier after
  // recursive descent.

  int32_t current_nesting = deepest_quantifier_nesting + 1;
  for(auto it = critical_path_end; it != quant_stack.end();) {
    prenex_quantifier_stack_entry& e = *it;
    if(e.nesting >= current_nesting) {
      it++;
      continue;
    }
    --current_nesting;
    critical_path.push_front(e);
    it = quant_stack.erase(it);
  }

  Strategy strategy(critical_path, quant_stack, o.get_mgr());
  std::list<prenex_quantifier_stack_entry>& result = strategy();

  for(prenex_quantifier_stack_entry& e : util::reverse(result)) {
    o = o.get_mgr().get(expression::op(e.t, e.var, o.get_id()));
  }
  return o;
}

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_quant(expression::op_ref o) {
  const auto old_v = o.get_mgr()[o->quant.v]->var;
  auto& old_v_obj = o.get_mgr().vars().getobj(old_v.v);

  uint32_t outer_bound = bounds_map[old_v.v];
  uint32_t bound = old_v_obj.counter++;
  bounds_map[old_v.v] = bound;

  auto bound_v =
    o.get_mgr().get(expression::op(expression::op_type::Var, old_v.v, bound));

  auto it = quant_stack.emplace(
    quant_stack.begin(),
    prenex_quantifier_stack_entry{
      (expression::op_type)o->type, bound_v.get_id(), quantifier_nesting });

  if(quantifier_nesting > deepest_quantifier_nesting) {
    deepest_quantifier_nesting = quantifier_nesting;
    critical_path_end = it;
  }

  ++quantifier_nesting;

  auto e = this->visit(o.get_mgr()[o->quant.e]);

  --quantifier_nesting;

  bounds_map[old_v.v] = outer_bound;

  return e;
}

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_exists(expression::op_ref o) {
  return walk_quant(o);
}

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_forall(expression::op_ref o) {
  return walk_quant(o);
}

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_var(expression::op_ref o) {
  auto it = bounds_map.find(o->var.v);
  if(it != bounds_map.end()) {
    return o.get_mgr().get(
      expression::op(expression::op_type::Var, o->var.v, it->second));
  }
  return o;
}

template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_not(expression::op_ref o) {
  auto begin_before_visit = quant_stack.begin();
  o = !this->visit(o.left());
  auto begin_after_visit = quant_stack.begin();
  if(begin_before_visit != begin_after_visit) {
    for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
      expression::op_type& t = it->t;
      t = expression::op_type_flip_quantifier(t);
    }
  }
  return o;
}

// This eliminates impls of the form ->
template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_impl(expression::op_ref o) {
  auto begin_before_visit = quant_stack.begin();
  auto left = !this->visit(o.left());
  auto begin_after_visit = quant_stack.begin();
  auto right = this->visit(o.right());
  if(begin_before_visit != begin_after_visit) {
    for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
      expression::op_type& t = it->t;
      t = expression::op_type_flip_quantifier(t);
    }
  }
  return o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
}

// This eliminates impls of the form <-
template<class Strategy>
expression::op_ref
prenex_quantifier<Strategy>::walk_lpmi(expression::op_ref o) {
  auto left = this->visit(o.left());
  auto begin_before_visit = quant_stack.begin();
  auto right = !this->visit(o.right());
  auto begin_after_visit = quant_stack.begin();
  if(begin_before_visit != begin_after_visit) {
    for(auto it = begin_after_visit; it != begin_before_visit; ++it) {
      expression::op_type& t = it->t;
      t = expression::op_type_flip_quantifier(t);
    }
  }
  return o.get_mgr().get(
    expression::op(expression::op_type::Or, left.get_id(), right.get_id()));
}

template struct prenex_quantifier<prenex_quantifier_Eup_Aup>;
template struct prenex_quantifier<prenex_quantifier_Eup_Adown>;
template struct prenex_quantifier<prenex_quantifier_Edown_Aup>;
template struct prenex_quantifier<prenex_quantifier_Edown_Adown>;
}

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e) {
  char leaf = e.subtree_leaf ? 'L' : 'I';
  return o << e.t << ":" << e.nesting << leaf;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}

std::ostream&
operator<<(
  std::ostream& o,
  const std::list<booleguru::transform::prenex_quantifier_stack_entry>& v) {
  bool first = true;
  for(const auto& e : v) {
    if(first) {
      first = false;
    } else {
      o << ", ";
    }
    o << e;
  }
  return o;
}
