#pragma once

namespace booleguru::expression {
class op_ref;
class script_ref;

class script_executor {
  public:
  virtual op_ref execute(script_ref script) = 0;
};
}
