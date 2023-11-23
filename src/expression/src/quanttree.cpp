#include "booleguru/expression/op.hpp"
#include <fstream>
#include <limits>
#include <stack>

#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/quanttree.hpp>
#include <booleguru/expression/var_manager.hpp>

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree& q) {
  bool first = true;
  for(size_t i = 0; i < q.size(); ++i) {
    if(first)
      first = false;
    else
      o << ", ";

    q[i].stream(o, nullptr);
  }
  return o;
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::entry& e) {
  return e.stream(o, nullptr);
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::path& p) {
  o << p.type << ":" << p.var;
  if(p.has_next())
    o << "." << p.next;
  return o;
}

namespace booleguru::expression {
std::ostream&
booleguru::expression::quanttree::entry::stream(std::ostream& o,
                                                const op_manager* ops) const {
  if(is_fork()) {
    o << f.left << ":" << f.right;
  } else {
    o << p.type << ":";
    if(ops && p.var < ops->size()) {
      auto varop = (*ops)[p.var];
      o << varop;
    } else {
      o << p.var;
    }
  }
  o << " (" << alternations << " QAs)";
  return o;
}

void
quanttree::set_lookup_op_manager(const op_manager* ops) {
  this->ops = ops;
}

uint32_t
quanttree::add(op_type quant_type, uint32_t var, uint32_t next) {
  size_t s = v.size();

  ++number_of_quantifiers;

  op_type t = should_flip() ? op_type_flip_quantifier(quant_type) : quant_type;
  if(v.emplace_back(t, var, next).has_next()) {
    assert(next < v.size());
    v[next].parent_ = s;

    bool increment = false;
    if(v[next].is_fork()) {
      std::stack<uint32_t> unvisited;
      unvisited.emplace(next);
      while(!unvisited.empty() && !increment) {
        uint32_t i = unvisited.top();
        entry& e = v[i];
        unvisited.pop();
        if(e.is_fork_) {
          entry& l = v[e.f.left];
          entry& r = v[e.f.right];
          if(l.alternations >= r.alternations) {
            unvisited.emplace(e.f.left);
          }
          if(l.alternations <= r.alternations) {
            unvisited.emplace(e.f.right);
          }
        } else {
          increment = t != e.p.type;
        }
      }
    } else {
      increment = t != v[next].p.type;
    }
    if(increment) {
      v[s].alternations = v[next].alternations + 1;
    } else {
      v[s].alternations = v[next].alternations;
    }
  }

  return s;
}

uint32_t
quanttree::add(op_type quant_type, uint32_t var) {
  return add(quant_type, var, std::numeric_limits<uint32_t>::max());
}

uint32_t
quanttree::add(uint32_t left, uint32_t right) {
  if(left == std::numeric_limits<uint32_t>::max()
     && right == std::numeric_limits<uint32_t>::max()) {
    return std::numeric_limits<uint32_t>::max();
  }
  if(left != std::numeric_limits<uint32_t>::max()
     && right == std::numeric_limits<uint32_t>::max()) {
    return left;
  }
  if(left == std::numeric_limits<uint32_t>::max()
     && right != std::numeric_limits<uint32_t>::max()) {
    return right;
  }
  size_t s = v.size();
  v.emplace_back(left, right);
  assert(left < v.size());
  assert(right < v.size());
  v[left].parent_ = s;
  v[right].parent_ = s;

  v[s].alternations = std::max(v[left].alternations, v[right].alternations);

  return s;
}

void
quanttree::flip_downwards(uint32_t start) {
  assert(start != std::numeric_limits<uint32_t>::max());
  std::stack<uint32_t> unvisited;
  unvisited.emplace(start);

  while(!unvisited.empty()) {
    uint32_t i = unvisited.top();
    unvisited.pop();

    entry& e = v[i];
    if(e.is_fork_) {
      unvisited.emplace(e.f.left);
      unvisited.emplace(e.f.right);
    } else {
      e.p.type = op_type_flip_quantifier(e.p.type);
      if(e.has_next())
        unvisited.emplace(e.p.next);
    }
  }
}

void
quanttree::correct_QA_diff(uint32_t path, uint32_t insert, uint32_t last) {
  // Update the number of quantifier alternations between insert->path to match
  // the level at path.
  uint32_t qa_diff = v[path].alternations - v[insert].alternations;
  if(qa_diff > 0) {
    for(uint32_t i = insert; i != v[last].p.next; i = next_marked(i)) {
      v[i].alternations += qa_diff;
    }
  }
}

void
quanttree::splice_path_after_path(uint32_t path, uint32_t insert) {
  assert(v[path].is_path());
  assert(v[insert].is_path());
  assert(v[v[insert].parent_].is_fork());

  mark_critical_path(insert);

  op_type t = v[insert].p.type;

  uint32_t last = last_entry_on_critical_path_with_quantifier(insert, t);

  correct_QA_diff(path, insert, last);

  if(v[last].has_next()) {
    // Move the fork that we got our insert from to the position right after the
    // insert, i.e. after last.
    uint32_t fork = v[insert].parent_;
    assert(v[fork].is_fork_);

    remove_entry(insert);

    uint32_t next_r = v[last].p.next;
    uint32_t next_l = v[path].p.next;

    if(next_l == std::numeric_limits<uint32_t>::max()) {
      // Don't continue anymore, as there is nothing else to continue to! Just
      // insert there.
      v[last].p.next = next_r;
      v[next_r].parent_ = last;
    } else {
      unmark(next_r);
      v[fork].is_fork_ = true;
      v[last].p.next = fork;
      v[fork].parent_ = last;
      v[fork].f.left = next_l;
      v[fork].f.right = next_r;
      v[fork].marked_ = true;
      assert(!v[next_r].marked_);
      v[next_l].parent_ = fork;
      v[next_r].parent_ = fork;
    }
  } else {
    remove_entry(insert);

    v[last].p.next = v[path].p.next;
    if(v[path].has_next()) {
      v[v[path].p.next].parent_ = last;
    }
  }
  v[insert].parent_ = path;
  v[path].p.next = insert;
}

uint32_t
quanttree::splice_path_before_path(uint32_t path, uint32_t insert) {
  assert(v[path].is_path());
  assert(v[insert].is_path());
  assert(v[v[insert].parent_].is_fork());

  mark_critical_path(insert);

  op_type t = v[insert].p.type;

  uint32_t last = last_entry_on_critical_path_with_quantifier(insert, t);
  uint32_t returned = last;

  correct_QA_diff(path, insert, last);

  if(v[last].has_next()) {
    // Move the fork that we got our insert from to the position right after the
    // insert, i.e. after last. Similar as in splice_after.
    uint32_t fork = v[insert].parent_;
    assert(v[fork].is_fork_);

    remove_entry(insert);

    uint32_t next_r = v[last].p.next;
    uint32_t next_l = v[path].p.next;

    if(next_l == std::numeric_limits<uint32_t>::max()) {
      // Don't continue anymore, as there is nothing else to continue to! Just
      // insert there.
      v[last].p.next = next_r;
      v[next_r].parent_ = last;
      returned = last_entry_on_critical_path(insert);
    } else {
      unmark(next_r);
      v[fork].is_fork_ = true;
      v[last].p.next = fork;
      v[fork].parent_ = last;
      v[fork].f.left = next_l;
      v[fork].f.right = next_r;
      v[fork].marked_ = true;
      assert(!v[next_r].marked_);
      v[next_l].parent_ = fork;
      v[next_r].parent_ = fork;
    }
  } else {
    remove_entry(insert);
    v[last].p.next = v[path].p.next;
    if(v[path].has_next())
      v[v[path].p.next].parent_ = last;
  }

  v[insert].parent_ = path;
  v[path].p.next = insert;

  return returned;
}

void
quanttree::remove_entry(uint32_t entry, uint32_t next) {
  quanttree::entry& e = v[entry];
  quanttree::entry& parent = v[e.parent_];
  quanttree::entry& n = v[next];
  assert(e.has_parent());

  if(parent.is_fork()) {
    if(parent.f.left == entry) {
      parent.f.left = next;
    }
    if(parent.f.right == entry) {
      parent.f.right = next;
    }
    n.parent_ = e.parent_;
  } else {
    parent.p.next = next;
    n.parent_ = e.parent_;
  }
}

void
quanttree::remove_entry(uint32_t entry) {
  quanttree::entry& e = v[entry];
  quanttree::entry& parent = v[e.parent_];
  assert(e.has_parent());

  if(parent.is_fork()) {
    parent.is_fork_ = false;
    if(parent.f.left == entry) {
      remove_entry(e.parent_, parent.f.right);
    }
    if(parent.f.right == entry) {
      remove_entry(e.parent_, parent.f.left);
    }
  } else {
    parent.void_next();
  }
}

uint32_t
quanttree::marked_contains_forks(uint32_t i) {
  assert(!v[0].is_fork());

  while(i < v.size()) {
    entry& e = v[i];
    if(e.is_fork_)
      return i;
    i = e.p.next;
  }
  return 0;
}

uint32_t
quanttree::next_highest_QAs(uint32_t i) {
  const entry& e = v[i];

  if(e.is_path()) {
    return e.p.next;
  }

  uint32_t left_ = e.f.left;
  entry& left = v[left_];
  uint32_t right_ = e.f.right;
  entry& right = v[right_];

  size_t left_alternations = left.alternations;
  size_t right_alternations = right.alternations;

  assert(!left.marked_);
  assert(!right.marked_);

  if(left_alternations == right_alternations) {
    uint32_t p = last_path(i);
    if(p != std::numeric_limits<uint32_t>::max()) {
      entry& pe = v[p];

      // Check which side gives us the most quantifier alternations. This may
      // also include a changing quantifier from the last path to the next,
      // which is why we have to walk over all possibilities. This only applies
      // if the alternations would otherwise be equal.

      bool left_found = false;
      walk_next_paths(left,
                      [&pe, &left_alternations, &left_found](const entry& e) {
                        if(e.p.type != pe.p.type && !left_found) {
                          ++left_alternations;
                          left_found = true;
                        }
                      });

      bool right_found = false;
      walk_next_paths(right,
                      [&pe, &right_alternations, &right_found](const entry& e) {
                        if(e.p.type != pe.p.type && !right_found) {
                          ++right_alternations;
                          right_found = true;
                        }
                      });
    }

    if(left_alternations == right_alternations) {
      // Still, there is an equal amount of alternations. Then we should
      // prioritize the prioritized quantifier first and go down in that
      // direction.
      entry& pe = v[p];

      bool left_is_prioritized = false;
      walk_next_paths(left, [this, &pe, &left_is_prioritized](const entry& e) {
        if(e.p.type == prioritized_quantifier && !left_is_prioritized) {
          left_is_prioritized = true;
        }
      });
      if(left_is_prioritized) {
        return left_;
      } else {
        bool right_is_prioritized = false;
        walk_next_paths(
          left, [this, &pe, &right_is_prioritized](const entry& e) {
            if(e.p.type == prioritized_quantifier && !right_is_prioritized) {
              right_is_prioritized = true;
            }
          });
        return right_;
      }
    }
  }

  if(left_alternations > right_alternations) {
    return left_;
  } else {
    return right_;
  }
}

uint32_t
quanttree::next_marked(uint32_t i) {
  const entry& e = v[i];

  if(e.is_path()) {
    uint32_t next = e.p.next;
    return next;
  }

  uint32_t left_ = e.f.left;
  assert(left_ != i);
  const entry& left = v[left_];
  uint32_t right_ = e.f.right;
  assert(right_ != i);
  const entry& right = v[right_];
  if(left.marked_) {
    return left_;
  } else if(right.marked_) {
    return right_;
  } else {
    assert(false);
  }
}

uint32_t
quanttree::next_unmarked(const entry& e) {
  assert(e.is_fork_);
  uint32_t left_ = e.f.left;
  assert(left_ != e.index(v.data()));
  const entry& left = v[left_];
  uint32_t right_ = e.f.right;
  assert(right_ != e.index(v.data()));
  const entry& right = v[right_];
  if(!left.marked_) {
    return left_;
  } else if(!right.marked_) {
    return right_;
  } else {
    assert(false);
  }
}

uint32_t
quanttree::next_unmarked_path(uint32_t i) {
  while(v[i].is_fork_) {
    i = next_unmarked(v[i]);
  }
  return i;
}

uint32_t
quanttree::last_path(uint32_t i) {
  assert(i < size());
  while(i < size() && v[i].is_fork_) {
    i = v[i].parent_;
  }
  return i;
}

void
quanttree::prenex(uint32_t root, should_inline_checker should_inline) {
  if(animate)
    create_animation_step(root);
  mark_critical_path(root);
  if(animate)
    create_animation_step(root);

  if(ensure_root_is_path(root) && animate)
    create_animation_step(root);

  uint32_t bottom = 0;
  bool changing = false;
  bool ignore_QAs = false;

  do {
    changing = false;
    for(uint32_t c = root; c < size(); c = next_marked(c)) {
      entry& e = v[c];
      if(e.is_fork())
        continue;

      bool is_above = true;
      for(uint32_t f = root; f < size() && (is_above || last_path(f) == c);
          f = next_marked(f)) {

        assert(v[f].marked_);

        if(f == c)
          is_above = false;
        if(!v[f].is_fork_)
          continue;

        walk_next_paths(
          v[f],
          [this, root, &should_inline, &e, c, &changing, &is_above](
            entry& check) {
            if(should_inline(direction::downwards, e, check)) {
              splice_path_after_path(c, index(check));
              if(animate)
                create_animation_step(root);
              changing = true;
              is_above = false;
            }
          });
      }
      bottom = c;
    }

    if(marked_contains_forks(root)) {
      for(;;) {
        uint32_t next_override = std::numeric_limits<uint32_t>::max();
        std::reference_wrapper<entry> e = v[bottom];
        if(e.get().is_fork()) {
          bottom = e.get().parent_;
          continue;
        }

        bool is_below = true;

        for(uint32_t f = root;
            f < size() && (is_below || last_path(f) == bottom);
            f = next_marked(f)) {
          if(f == bottom)
            is_below = false;
          if(!v[f].is_fork_)
            continue;

          walk_next_paths(
            v[f],
            [this,
             root,
             &should_inline,
             &e,
             &bottom,
             &next_override,
             &changing,
             ignore_QAs,
             &is_below](entry& check) {
              if(!ignore_QAs && e.get().alternations < check.alternations)
                return;

              if(should_inline(direction::upwards, e, check)) {
                next_override = splice_path_before_path(bottom, index(check));
                if(animate)
                  create_animation_step(root);
                changing = true;
                e = v[next_override];
                is_below = false;
              }
            });

          if(next_override != std::numeric_limits<uint32_t>::max()) {
            break;
          }
        }
        if(next_override != std::numeric_limits<uint32_t>::max()) {
          bottom = next_override;
        } else if(e.get().has_parent())
          bottom = e.get().parent_;
        else
          break;
      }
    }
    if(!changing && ignore_QAs) {
      ignore_QAs = false;
      // Nothing helped! We have to splice the remaining fork at the very
      // bottom.
      if(uint32_t f = marked_contains_forks(root)) {
        uint32_t last = last_entry_on_critical_path(root);
        splice_path_after_path(last, next_unmarked_path(f));
        if(animate)
          create_animation_step(root);
        changing = true;
      }
    } else if(!changing) {
      ignore_QAs = true;
    }
  } while((changing || ignore_QAs) && marked_contains_forks(root));
}

void
quanttree::mark_critical_path(uint32_t root) {
  assert(root < v.size());

  uint32_t i = root;

  while(i != std::numeric_limits<uint32_t>::max()) {
    v[i].mark();
    i = next_highest_QAs(i);
  }
}

bool
quanttree::ensure_root_is_path(uint32_t root) {
  if(v[root].is_fork_) {
    uint32_t next = next_path(root);

    uint32_t root_left = v[root].f.left;
    uint32_t root_right = v[root].f.right;
    v[root_left].parent_ = next;
    v[root_right].parent_ = next;

    assert(v[next].is_path());
    op_type next_type = v[next].p.type;
    uint32_t next_var = v[next].p.var;
    uint32_t next_parent = v[next].parent_;
    uint32_t next_after_next = next_marked(next);

    v[root].is_fork_ = false;
    v[root].p.next = next;
    v[root].p.type = next_type;
    v[root].p.var = next_var;

    v[next].is_fork_ = true;
    v[next].f.left = root_left;
    v[next].f.right = root_right;
    v[next].parent_ = root;

    if(v[next_parent].is_fork() && next_parent != root) {
      if(v[next_parent].f.left == next) {
        v[next_parent].f.left = next_after_next;
      } else if(v[next_parent].f.right == next) {
        v[next_parent].f.right = next_after_next;
      }

      // If there was no element afterwards, the fork disappears, getting
      // replaced by the next element.

      uint32_t other = std::numeric_limits<uint32_t>::max();

      if(v[next_parent].f.left == std::numeric_limits<uint32_t>::max()) {
        other = v[next_parent].f.right;
      } else if(v[next_parent].f.right
                == std::numeric_limits<uint32_t>::max()) {
        other = v[next_parent].f.left;
      }

      if(other != std::numeric_limits<uint32_t>::max()) {
        uint32_t pp = v[next_parent].parent_;
        v[other].parent_ = pp;

        if(v[pp].is_fork()) {
          if(v[pp].f.left == next_parent) {
            v[pp].f.left = other;
          } else if(v[pp].f.right == next_parent) {
            v[pp].f.right = other;
          } else {
            assert(false);
          }
        } else {
          v[pp].p.next = other;
        }

        // As the other branch was marked and was now removed, this other branch
        // has to be marked.
        mark_critical_path(other);
      }
    } else {
      v[next_parent].p.next = next_marked(next);

      if(v[next_parent].p.next != std::numeric_limits<uint32_t>::max()) {
        v[v[next_parent].p.next].parent_ = next_parent;
      }

      // The case that there is no element afterwards is covered by the logic of
      // paths.
    }
    return true;
  }
  return false;
}

void
quanttree::unmark(uint32_t root) {
  for(uint32_t i = root; i < size(); i = next_marked(i)) {
    v[i].marked_ = false;
  }
}

std::ostream&
quanttree::to_dot(std::string_view name, std::ostream& o, uint32_t root) {
  std::stack<uint32_t> unvisited;
  unvisited.emplace(root);

  o << "digraph " << name << " {\n";
  while(!unvisited.empty()) {
    uint32_t i = unvisited.top();
    unvisited.pop();
    const entry& e = v[i];
    o << i << " [ label=\"";
    e.stream(o, ops) << "^" << i << "\"";
    if(e.marked_) {
      o << ", color=\"red\"";
    }
    o << "];\n";

    if(e.is_fork()) {
      o << i << "->" << e.f.left << ";\n";
      o << i << "->" << e.f.right << ";\n";
      assert(e.f.left < v.size());
      unvisited.emplace(e.f.left);
      assert(e.f.right < v.size());
      unvisited.emplace(e.f.right);
    } else if(e.has_next()) {
      o << i << "->" << e.p.next << ";\n";
      unvisited.emplace(e.p.next);
    }
  }
  o << "}\n";
  return o;
}

bool
quanttree::should_inline_EupAup(direction dir,
                                const quanttree::entry& pos,
                                const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return false;
  return false;
}
bool
quanttree::should_inline_EdownAdown(direction dir,
                                    const quanttree::entry& pos,
                                    const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return false;
  if(dir == direction::upwards)
    return pos.p.type == possible_inline.p.type;
  return false;
}
bool
quanttree::should_inline_EupAdown(direction dir,
                                  const quanttree::entry& pos,
                                  const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == op_type::Exists
           && pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return pos.p.type == op_type::Forall
           && pos.p.type == possible_inline.p.type;
  return false;
}
bool
quanttree::should_inline_EdownAup(direction dir,
                                  const quanttree::entry& pos,
                                  const quanttree::entry& possible_inline) {
  assert(pos.is_path());
  assert(possible_inline.is_path());

  if(dir == direction::downwards)
    return pos.p.type == op_type::Forall
           && pos.p.type == possible_inline.p.type;
  if(dir == direction::upwards)
    return pos.p.type == op_type::Exists
           && pos.p.type == possible_inline.p.type;
  return false;
}

void
quanttree::activate_animation(const std::string& path) {
  animation_path = path;
  animate = true;
}

void
quanttree::create_animation_step(uint32_t root) {
  std::string name = "step" + std::to_string(animate_step);
  ++animate_step;
  std::ofstream f(animation_path + "_" + name + ".dot");
  to_dot(name, f, root);
}

op_ref
quanttree::prepend_marked_to_op(uint32_t root, op_ref o) {
  for(uint32_t i = last_entry_on_critical_path(root); i < size();
      i = v[i].parent_) {
    assert(v[i].is_path());
    path& p = v[i].p;
    o = o.get_mgr().get(expression::op(p.type, p.var, o.get_id()));
  }
  return o;
}
}
