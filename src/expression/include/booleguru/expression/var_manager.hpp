#pragma once

#include <memory>
#include <numeric>
#include <string>
#include <vector>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/manager.hpp>
#include <booleguru/expression/reference.hpp>

namespace booleguru::expression {
class op_manager;
class var_manager;
class script_manager;

struct variable {
  std::string name;
  mutable uint32_t counter = 0; /** Counter for things like tseitin-variables.
                                   Not part of hashing or equality. */

  inline size_t hash() const noexcept {
    return std::hash<decltype(name)>{}(name);
  }

  inline bool operator==(const variable& o) const noexcept {
    return name == o.name;
  }
};

class var_ref : public reference<variable, var_manager, var_id> {
  public:
  using reference<variable, var_manager, var_id>::reference;
};

class var_manager : public manager<var_ref, var_manager> {
  protected:
  inline void init() {
    auto top_id = get_id(variable{ "⊤" });
    auto bot_id = get_id(variable{ "⊥" });
    auto tseitin_id = get_id(variable{ "𝑡" });
    auto vec_id = get_id(variable{ "𝑣" });
    assert(top_id == LITERAL_TOP);
    assert(bot_id == LITERAL_BOTTOM);
    assert(tseitin_id == LITERAL_TSEITIN);
    assert(vec_id == LITERAL_VEC);
  }

  public:
  using base = manager<var_ref, var_manager>;
  using base::base;
  friend class manager;

  enum : uint32_t {
    LITERAL_TOP = 1,
    LITERAL_BOTTOM = 2,
    LITERAL_TSEITIN = 3,
    LITERAL_VEC = 4,
  };
};

std::ostream&
operator<<(std::ostream& o, const variable& v);

std::ostream&
operator<<(std::ostream& o, const var_ref& v);
}

namespace std {
template<>
struct hash<booleguru::expression::variable> {
  size_t operator()(const booleguru::expression::variable& x) const noexcept {
    return x.hash();
  }
};
}
