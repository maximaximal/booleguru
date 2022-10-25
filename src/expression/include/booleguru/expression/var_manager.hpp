#pragma once

#include <memory>
#include <string>

#include "manager.hpp"
#include "reference.hpp"

namespace booleguru::expression {
class op_manager;
class var_manager;
class script_manager;

struct variable {
  std::string name;

  inline size_t hash() const { return std::hash<decltype(name)>{}(name); }

  inline bool operator==(const variable& o) { return name == o.name; }
};

class var_ref : public reference<variable, var_manager> {
  public:
  using reference<variable, var_manager>::reference;
};

class var_manager : public manager<var_ref, var_manager> {
  public:
  using base = manager<var_ref, var_manager>;
};
}
