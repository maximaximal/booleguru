#pragma once

#include <string_view>

namespace booleguru::py {
struct check_sat_result {
  int res_ = 0;

  check_sat_result() {}
  check_sat_result(int res)
    : res_(res) {}

  std::string_view to_string() {
    switch(res_) {
      case 10:
        return "Sat";
      case 20:
        return "Unsat";
      default:
        return "Unknown";
    }
  }

  inline bool operator==(const check_sat_result& o) const {
    return res_ == o.res_;
  }
};
}
