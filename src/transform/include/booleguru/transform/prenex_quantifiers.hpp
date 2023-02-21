#pragma once

#include "visitor.hpp"
#include <algorithm>
#include <booleguru/expression/op.hpp>
#include <booleguru/util/reverse.hpp>

#include <iosfwd>
#include <iterator>
#include <limits>
#include <list>
#include <unordered_map>

#include <iostream>

namespace booleguru::transform {
struct prenex_quantifier_stack_entry {
  expression::op_type t;
  uint32_t var;
  int32_t nesting;
  bool subtree_leaf = false;

  [[nodiscard]] static bool inline is_exists(
    const prenex_quantifier_stack_entry& e) noexcept {
    return e.t == expression::op_type::Exists;
  }
  [[nodiscard]] static bool inline is_forall(
    const prenex_quantifier_stack_entry& e) noexcept {
    return e.t == expression::op_type::Forall;
  }

  static void mark_leaves(std::list<prenex_quantifier_stack_entry>& l) {
    std::list<prenex_quantifier_stack_entry>::iterator last_it = l.end();
    for(auto it = l.begin(); it != l.end(); ++it) {
      if(last_it != l.end()) {
        last_it->subtree_leaf = last_it->nesting >= it->nesting;

        // Same element on the leaf must also be a leaf. So, once a leaf is
        // found, walk backwards on the same sub-tree until there is some other
        // element with a different op_type, which is when the backwards-walk
        // ends.
        if(last_it->subtree_leaf) {
          auto rit = make_reverse_iterator(last_it);
          ++rit;
          while(rit != l.rend() && rit->t == last_it->t &&
                rit->nesting + 1 == last_it->nesting) {
            rit->subtree_leaf = true;
            ++rit;
          }
        }
      }
      last_it = it;
    }
    // Last entry MUST be a leaf.
    l.rbegin()->subtree_leaf = true;
  }
};
}

std::ostream&
operator<<(std::ostream& o,
           const booleguru::transform::prenex_quantifier_stack_entry& e);

std::ostream&
operator<<(
  std::ostream& o,
  const std::vector<booleguru::transform::prenex_quantifier_stack_entry>& e);

std::ostream&
operator<<(
  std::ostream& o,
  const std::list<booleguru::transform::prenex_quantifier_stack_entry>& e);

namespace booleguru::transform {
struct prenex_quantifier_Eup_Aup {
  std::list<prenex_quantifier_stack_entry>& critical_path;
  std::list<prenex_quantifier_stack_entry>& remaining;
  expression::op_manager& ops;

  inline constexpr prenex_quantifier_Eup_Aup(
    std::list<prenex_quantifier_stack_entry>& critical_path,
    std::list<prenex_quantifier_stack_entry>& remaining,
    expression::op_manager& ops) noexcept
    : critical_path(critical_path)
    , remaining(remaining)
    , ops(ops) {}

  [[nodiscard]] inline std::list<prenex_quantifier_stack_entry>& operator()() {
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

        //  std::cout << "Found in remaining: " << ops[it->var] << ":" << *it
        //           << std::endl;

        if(it->t == expression::op_type::Forall) {
          ignored_foralls.emplace_back(*it);
        } else {
          auto next_cit = cit;
          ++next_cit;
          cit = critical_path.emplace(next_cit, *it);
        }

        remaining.erase(it);
      }

      if(cit == critical_path.end()) {
        break;
      }
    }

    remaining.splice(remaining.end(), ignored_foralls);

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

        //  std::cout << "Found in remaining: " << ops[it->var] << ":" << *it
        //           << std::endl;

        if(it->t == expression::op_type::Exists) {
          ignored_exists.emplace_back(*it);
        } else {
          auto next_cit = cit;
          ++next_cit;
          cit = critical_path.emplace(next_cit, *it);
        }

        remaining.erase(it);
      }

      if(cit == critical_path.end()) {
        break;
      }
    }

    remaining.splice(remaining.end(), ignored_exists);

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

template<class Strategy = prenex_quantifier_Eup_Aup>
struct prenex_quantifier : public visitor<prenex_quantifier<Strategy>> {
  std::unordered_map<uint32_t, uint32_t> bounds_map;
  int32_t quantifier_nesting = 0;
  int32_t deepest_quantifier_nesting = std::numeric_limits<int32_t>::min();

  using quant_stack_t = std::list<prenex_quantifier_stack_entry>;
  quant_stack_t quant_stack;
  quant_stack_t::iterator critical_path_end;
  std::unordered_map<uint32_t, uint32_t> variable_counters;

  inline expression::op_ref post_action(expression::op_ref o) {
    std::list<prenex_quantifier_stack_entry> critical_path;

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

  inline expression::op_ref walk_quant(expression::op_ref o) {
    const auto old_v = o.get_mgr()[o->quant.v]->var;

    uint32_t outer_bound = bounds_map[old_v.v];
    uint32_t bound = variable_counters[old_v.v]++;
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

  inline expression::op_ref walk_exists(expression::op_ref o) {
    return walk_quant(o);
  }

  inline expression::op_ref walk_forall(expression::op_ref o) {
    return walk_quant(o);
  }

  inline expression::op_ref walk_var(expression::op_ref o) {
    auto it = bounds_map.find(o->var.v);
    if(it != bounds_map.end()) {
      return o.get_mgr().get(
        expression::op(expression::op_type::Var, o->var.v, it->second));
    }
    return o;
  }

  inline expression::op_ref walk_not(expression::op_ref o) {
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
};
}
