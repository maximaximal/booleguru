#pragma once

#include <cassert>
#include <limits>
#include <ostream>
#include <vector>

#include "op.hpp"

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
    uint32_t alternations = 0;
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

  using should_inline_checker =
    bool (*)(direction dir,
             const quanttree::entry& pos,
             const quanttree::entry& possible_inline);

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

  constexpr void close_flip_ctx() {
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

  void flip_downwards(uint32_t start);

  flip_ctx open_flip_ctx() {
    ++flip_ctx_count;
    return flip_ctx(*this);
  }

  void increment_flip_count() { ++flip_ctx_count; }
  void decrement_flip_count() {
    assert(flip_ctx_count > 0);
    --flip_ctx_count;
  }

  [[nodiscard]] static bool consteval inline is_exists(
    const entry& e) noexcept {
    return e.is_exists();
  }
  [[nodiscard]] static bool consteval inline is_forall(
    const entry& e) noexcept {
    return e.is_forall();
  }

  quanttree(size_t reserve = 256) {
    v.reserve(reserve);
    v.emplace_back(op_type::Exists,
                   std::numeric_limits<uint32_t>::max(),
                   std::numeric_limits<uint32_t>::max());
  }

  [[nodiscard]] inline size_t size() const noexcept { return v.size(); }
  [[nodiscard]] inline constexpr entry& operator[](uint32_t i) noexcept {
    return v[i];
  }
  [[nodiscard]] inline const entry& operator[](uint32_t i) const noexcept {
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
  uint32_t splice_path_before_path(uint32_t path, uint32_t insert);

  /** @brief Removes the entry, replacing its parent's next with next.
   */
  void remove_entry(uint32_t entry, uint32_t next);
  /** @brief Removes the entry.

      If the parent was a fork, the fork is destructed into a path going to the
     other direction.
   */
  void remove_entry(uint32_t entry);

  uint32_t marked_contains_forks(uint32_t root);

  uint32_t next_highest_QAs(uint32_t i);

  uint32_t next_marked(uint32_t i);

  uint32_t next_unmarked(const entry& e);

  uint32_t last_path(uint32_t i);

  void correct_QA_diff(uint32_t path, uint32_t insert, uint32_t last);

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

  uint32_t last_entry_on_critical_path_with_quantifier(uint32_t i, op_type t) {
    while(v[i].is_fork_ || (v[i].has_next() && v[next_path(i)].p.type == t))
      i = next_marked(i);
    return i;
  }

  void activate_animation(const std::string& path);

  op_ref prepend_marked_to_op(uint32_t root, op_ref o);

  template<typename Functor>
  void prenex(uint32_t root, Functor should_inline) {
    mark_critical_path(root);
    if(animate)
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

          if(f == c)
            is_above = false;
          if(!v[f].is_fork_)
            continue;

          walk_next_paths(
            v[f], [this, root, &should_inline, &e, c, &changing](entry& check) {
              if(should_inline(direction::downwards, e, check)) {
                splice_path_after_path(c, index(check));
                if(animate)
                  create_animation_step(root);
                changing = true;
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
               ignore_QAs](entry& check) {
                if(!ignore_QAs && e.get().alternations < check.alternations)
                  return;

                if(should_inline(direction::upwards, e, check)) {
                  next_override = splice_path_before_path(bottom, index(check));
                  if(animate)
                    create_animation_step(root);
                  changing = true;
                  e = v[next_override];
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
      ignore_QAs = false;
      if(!changing) {
        ignore_QAs = true;
      }
    } while((changing || ignore_QAs) && marked_contains_forks(root));
  }

  void mark_critical_path(uint32_t root);

  void unmark(uint32_t root);

  uint32_t index(const entry& e) const { return e.index(v.data()); }

  static bool should_inline_EupAup(direction dir,
                                   const quanttree::entry& pos,
                                   const quanttree::entry& possible_inline);
  static bool should_inline_EdownAdown(direction dir,
                                       const quanttree::entry& pos,
                                       const quanttree::entry& possible_inline);
  static bool should_inline_EupAdown(direction dir,
                                     const quanttree::entry& pos,
                                     const quanttree::entry& possible_inline);
  static bool should_inline_EdownAup(direction dir,
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
