#pragma once

#include <memory>
#include <string>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/manager.hpp>
#include <booleguru/expression/reference.hpp>

namespace booleguru::expression {
class op_manager;
class var_manager;
class script_manager;

struct script {
  enum type { lua, python, cl };

  std::string code;
  type kind;

  inline size_t hash() const {
    return 4017271 * kind + 70200511 * std::hash<decltype(code)>{}(code);
  }

  inline bool operator==(const script& o) { return code == o.code; }
};

class script_ref : public reference<script, script_manager, script_id> {
  public:
  using reference<script, script_manager, script_id>::reference;
};

class script_manager : public manager<script_ref, script_manager> {
  public:
  using base = manager<script_ref, script_manager>;
  using base::base;
};
}
