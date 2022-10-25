#pragma once

#include <memory>
#include <string>

#include "manager.hpp"
#include "reference.hpp"

namespace booleguru::expression {
class op_manager;
class var_manager;
class script_manager;

struct script {
  enum type { lua, python, cl };

  std::string code;
  type kind;

  inline size_t hash() const { return std::hash<decltype(code)>{}(code); }

  inline bool operator==(const script& o) { return code == o.code; }
};

class script_ref : public reference<script, script_manager> {
  public:
  using reference<script, script_manager>::reference;
};

class script_manager : public manager<script_ref, script_manager> {
  public:
  using base = manager<script_ref, script_manager>;
};
}
