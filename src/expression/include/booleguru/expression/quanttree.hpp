#pragma once

#include <cassert>
#include <ostream>
#include <vector>

#include "op.hpp"

namespace booleguru::expression {
/** Toolkit for manipulating quantifier trees */
class quanttree {
  public:
  struct path {
    uint32_t var;
    uint32_t next;
    uint32_t parent = std::numeric_limits<uint32_t>::max();
    bool is_fork = false;
    bool marked = false;
    op_type type;

    explicit path(op_type op, uint32_t var, uint32_t next) noexcept
      : var(var)
      , next(next)
      , type(op) {}

    [[nodiscard]] bool constexpr inline has_next() const noexcept {
      return next != std::numeric_limits<uint32_t>::max();
    }
  };
  struct fork {
    uint32_t left;
    uint32_t right;
    uint32_t parent = std::numeric_limits<uint32_t>::max();
    bool is_fork = true;
    bool marked = false;
    op_type type;

    explicit fork(uint32_t left, uint32_t right) noexcept
      : left(left)
      , right(right)
      , is_fork(true) {}
  };

  union entry {
    path p;
    fork f;

    [[nodiscard]] constexpr inline bool is_fork() const noexcept {
      return p.is_fork;
    }
    [[nodiscard]] constexpr inline bool is_path() const noexcept {
      return !p.is_fork;
    }

    [[nodiscard]] bool constexpr inline is_exists() const noexcept {
      assert(is_path());
      return p.type == op_type::Exists;
    }
    [[nodiscard]] bool constexpr inline is_forall() const noexcept {
      assert(is_path());
      return p.type == op_type::Forall;
    }
    [[nodiscard]] bool constexpr inline has_next() const noexcept {
      assert(is_path());
      return p.has_next();
    }
    [[nodiscard]] bool constexpr inline has_parent() const noexcept {
      return p.parent != std::numeric_limits<uint32_t>::max();
    }

    void constexpr inline void_next() noexcept {
      p.next = std::numeric_limits<uint32_t>::max();
    }

    inline void constexpr mark() noexcept { p.marked = true; }

    explicit entry(op_type op_type, uint32_t var, uint32_t next) noexcept
      : p(op_type, var, next) {}
    explicit entry(uint32_t left, uint32_t right) noexcept
      : f(left, right) {}

    constexpr uint32_t index(const entry* arr) const { return this - arr; }

    std::ostream& stream(std::ostream&) const;
  };

  private:
  using qvec_t = std::vector<entry>;
  qvec_t v;

  uint32_t number_of_quantifiers = 0;
  int flip_ctx_count = 0;

  [[nodiscard]] inline bool should_flip() const noexcept {
    return flip_ctx_count % 2 == 1;
  }

  void close_flip_ctx() {
    assert(flip_ctx_count > 0);
    --flip_ctx_count;
  }

  public:
  struct flip_ctx {
    inline constexpr explicit flip_ctx(quanttree& v) noexcept
      : v_(v) {}
    inline constexpr ~flip_ctx() noexcept { v_.close_flip_ctx(); }

    inline constexpr flip_ctx(flip_ctx&) = delete;
    inline constexpr flip_ctx(flip_ctx&&) = delete;

    private:
    quanttree& v_;
  };

  flip_ctx open_flip_ctx() {
    ++flip_ctx_count;
    return flip_ctx(*this);
  }

  [[nodiscard]] static bool consteval inline is_exists(
    const entry& e) noexcept {
    return e.is_exists();
  }
  [[nodiscard]] static bool consteval inline is_forall(
    const entry& e) noexcept {
    return e.is_forall();
  }

  quanttree(size_t reserve = 256) { v.reserve(reserve); }

  [[nodiscard]] inline constexpr size_t size() const noexcept {
    return v.size();
  }
  [[nodiscard]] inline constexpr entry& operator[](uint32_t i) noexcept {
    return v[i];
  }
  [[nodiscard]] inline constexpr const entry& operator[](
    uint32_t i) const noexcept {
    return const_cast<quanttree&>(*this)[i];
  }

  uint32_t add(op_type quant_type, uint32_t var, uint32_t next);
  uint32_t add(op_type quant_type, uint32_t var);
  uint32_t add(uint32_t left, uint32_t right);

  using quantvec = std::vector<uint32_t>;

  template<typename Functor>
  void walk_next_paths(entry& e, Functor f) {
    if(e.is_fork()) {
      walk_next_paths(v[e.f.left], f);
      walk_next_paths(v[e.f.right], f);
    } else {
      f(e);
    }
  }

  /** @brief Remove a path and insert it into a fork.

      The parent fork of the path is destructed.

      The path to be inserted is inserted before the target fork.

      If the target fork and the path's parent fork are the same, the target
      fork is effectively replaced entirely with the path.
   */
  void splice_path_into_fork(uint32_t path, uint32_t fork);

  /** @brief Removes the entry, replacing its parent's next with next.
   */
  void remove_entry(uint32_t entry, uint32_t next);
  /** @brief Removes the entry.

      If the parent was a fork, the fork is destructed into a path going to the
     other direction.
   */
  void remove_entry(uint32_t entry);

  template<typename Functor>
  void prenex(const quantvec& critical_path, Functor f) {
    path* last_critical_path_entry = &v[critical_path[0]].p;
    for(size_t ci = 0; ci < critical_path.size(); ++ci) {
      entry& e = v[critical_path[ci]];

      // The critical path has alternating forks and paths. When encountering a
      // fork, one may decide to "pull in" the sibling.

      if(e.is_fork()) {
        // Ask the functor for every given path if it should be prenexed at this
        // position.
        entry& next =
          v[e.f.left == critical_path[ci + 1] ? e.f.right : e.f.left];
        uint32_t next_in_critical_path =
          e.f.left == critical_path[ci + 1] ? e.f.left : e.f.right;
        walk_next_paths(
          next,
          [this, &f, &e, next_in_critical_path, &last_critical_path_entry](
            entry& check) {
            if(f(*last_critical_path_entry, check.p)) {
              assert(e.has_parent());
              assert(!check.is_fork());
              v[e.f.parent].p.next = index(check);

              splice_path_into_fork(index(check), index(e));
            }
          });
      } else {
        path& p = v[critical_path[ci]].p;
        last_critical_path_entry = &p;
      }
    }
  }

  quantvec compute_critical_path(uint32_t root);

  uint32_t index(const entry& e) const { return e.index(v.data()); }

  static bool should_inline_EupAup(quanttree::path& pos,
                                   quanttree::path& possible_inline);

  std::ostream& to_dot(std::ostream& o);
  std::ostream& to_dot(std::ostream& o, uint32_t root);
  std::ostream& to_dot(std::ostream& o, quantvec v);
};
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::path& p);

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::entry& e);

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree& q);
