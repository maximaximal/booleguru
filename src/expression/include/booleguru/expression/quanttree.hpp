#pragma once

#include <cassert>
#include <ostream>
#include <vector>

#include "op.hpp"

#include <iostream>

namespace booleguru::expression {
/** Toolkit for manipulating quantifier trees */
class quanttree {
  public:
  enum class direction { upwards, downwards };

  struct path {
    uint32_t var;
    uint32_t next;
    op_type type;

    explicit path(op_type type, uint32_t var, uint32_t next) noexcept
      : var(var)
      , next(next)
      , type(type) {}

    [[nodiscard]] bool constexpr inline has_next() const noexcept {
      return next != std::numeric_limits<uint32_t>::max();
    }
  };
  struct fork {
    uint32_t left;
    uint32_t right;
    bool left_critical = false;

    explicit fork(uint32_t left, uint32_t right) noexcept
      : left(left)
      , right(right) {}
  };

  struct entry {
    union {
      path p;
      fork f;
    };

    uint32_t parent_ = std::numeric_limits<uint32_t>::max();
    bool is_fork_ : 1 = true;
    bool marked_ : 1 = false;

    [[nodiscard]] constexpr inline bool is_fork() const noexcept {
      return is_fork_;
    }
    [[nodiscard]] constexpr inline bool is_path() const noexcept {
      return !is_fork_;
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
      return parent_ != std::numeric_limits<uint32_t>::max();
    }

    void constexpr inline void_next() noexcept {
      p.next = std::numeric_limits<uint32_t>::max();
    }

    inline void constexpr mark() noexcept { marked_ = true; }

    explicit entry(op_type op_type, uint32_t var, uint32_t next) noexcept
      : p(op_type, var, next)
      , is_fork_(false) {}
    explicit entry(uint32_t left, uint32_t right) noexcept
      : f(left, right) {}

    constexpr uint32_t index(const entry* arr) const { return this - arr; }

    std::ostream& stream(std::ostream&) const;
  };

  private:
  using qvec_t = std::vector<entry>;
  qvec_t v;
  std::string animation_path = "";
  bool animate = false;
  uint32_t animate_step = 0;

  void create_animation_step(uint32_t root);

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
      if(!v[e.f.left].marked_)
        walk_next_paths(v[e.f.left], f);
      if(!v[e.f.right].marked_)
        walk_next_paths(v[e.f.right], f);
    } else {
      f(e);
    }
  }

  void splice_path_after_path(uint32_t path, uint32_t insert);
  void splice_path_before_path(uint32_t path, uint32_t insert);

  /** @brief Removes the entry, replacing its parent's next with next.
   */
  void remove_entry(uint32_t entry, uint32_t next);
  /** @brief Removes the entry.

      If the parent was a fork, the fork is destructed into a path going to the
     other direction.
   */
  void remove_entry(uint32_t entry);

  uint32_t marked_contains_forks(uint32_t root);

  uint32_t next_marked(uint32_t i) {
    const entry& e = v[i];

    if(e.is_path()) {
      return e.p.next;
    }

    uint32_t left_ = e.f.left;
    const entry& left = v[left_];
    uint32_t right_ = e.f.right;
    const entry& right = v[right_];
    if(left.marked_) {
      return left_;
    } else if(right.marked_) {
      return right_;
    } else {
      assert(false);
    }
  }

  uint32_t next_unmarked(const entry& e) {
    assert(e.is_fork_);
    uint32_t left_ = e.f.left;
    const entry& left = v[left_];
    uint32_t right_ = e.f.right;
    const entry& right = v[right_];
    if(!left.marked_) {
      return left_;
    } else if(!right.marked_) {
      return right_;
    } else {
      assert(false);
    }
  }

  uint32_t last_path(uint32_t i) {
    assert(i < size());
    while(v[i].is_fork_) {
      i = v[i].parent_;
    }
    return i;
  }
  uint32_t next_path(uint32_t i) {
    assert(i < size());
    assert(v[i].is_fork_ || v[i].has_next());
    do {
      i = next_marked(i);
      assert(i < size());
    } while(v[i].is_fork_);
    return i;
  }

  uint32_t last_entry_on_critical_path(uint32_t i) {
    while(v[i].is_fork_ || v[i].has_next())
      i = next_marked(i);
    return i;
  }

  void activate_animation(const std::string& path);

  template<typename Functor>
  void prenex(uint32_t root, Functor should_inline) {
    mark_critical_path(root);
    if(animate)
      create_animation_step(root);

    uint32_t bottom = 0;
    for(uint32_t c = root; c < size(); c = next_marked(c)) {
      entry& e = v[c];
      if(e.is_fork())
        continue;

      bool is_above = true;
      for(uint32_t f = root; is_above || (f < size() && last_path(f) == c);
          f = next_marked(f)) {

        if(f == c)
          is_above = false;
        if(!v[f].is_fork_)
          continue;

        walk_next_paths(v[f],
                        [this, f, root, &should_inline, &e, c](entry& check) {
                          if(should_inline(direction::downwards, e, check)) {
                            splice_path_after_path(c, index(check));
                            if(animate)
                              create_animation_step(root);
                          }
                        });
      }
      bottom = c;
    }

    if(marked_contains_forks(root)) {
      for(;;) {
        entry& e = v[bottom];
        if(e.is_fork())
          continue;

        bool is_below = true;

        for(uint32_t f = root;
            f < size() && (is_below || last_path(f) == bottom);
            f = next_marked(f)) {
          if(f == bottom)
            is_below = false;
          if(!v[f].is_fork_)
            continue;

          walk_next_paths(
            v[f], [this, f, root, &should_inline, &e, bottom](entry& check) {
              if(should_inline(direction::upwards, e, check)) {
                splice_path_before_path(bottom, index(check));
                if(animate)
                  create_animation_step(root);
              }
            });
        }
        if(e.has_parent())
          bottom = e.parent_;
        else
          break;
      }
    }
  }

  void mark_critical_path(uint32_t root);

  uint32_t index(const entry& e) const { return e.index(v.data()); }

  static bool should_inline_EupAup(direction dir,
                                   const quanttree::entry& pos,
                                   const quanttree::entry& possible_inline);
  static bool should_inline_EdownAdown(direction dir,
                                       const quanttree::entry& pos,
                                       const quanttree::entry& possible_inline);

  std::ostream& to_dot(std::string_view name, std::ostream& o, uint32_t root);
};
}

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::path& p);

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree::entry& e);

std::ostream&
operator<<(std::ostream& o, const booleguru::expression::quanttree& q);
