#include <booleguru/transform/prenex_quantifiers.hpp>

#include <iostream>

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

  prenex_quantifier_Eup_Aup strategy(critical_path, quant_stack, o.get_mgr());
  std::list<prenex_quantifier_stack_entry>& result = strategy();

  for(prenex_quantifier_stack_entry& e : util::reverse(result)) {
    o = o.get_mgr().get(expression::op(e.t, e.var, o.get_id()));
  }
  return o;
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
