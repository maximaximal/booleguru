#pragma once

#include <cassert>
#include <limits>
#include <string>
#include <vector>

namespace booleguru::parse {
/** @brief Tracks s-expressions while parsing a stream.
 *
 *  Each opening ( opens a scope, each closing ) closes a scope. The current
 *  s-expr scope goes back as far as the last opening parenthesis is located.
 *  When a scope closes, data is forwarded. This works by having an
 *  std::string which is always appended to, except when the outermost scope
 *  closes, then the whole string is cleared.
 */

class sexpr_tracker {
  std::string str_;
  std::vector<size_t> scope_;
  size_t last_popped_scope_ = 0;

  public:
  sexpr_tracker() = default;
  ~sexpr_tracker() = default;

  inline void append(char c) {
    switch(c) {
      case '(':
        if(scope_.empty()) {
          str_.clear();
        }
        scope_.push_back(str_.size());
        str_.push_back('(');
        return;
      case ')': {
        if(scope_.empty()) {
          return;
        }
        last_popped_scope_ = scope_.back();
        scope_.pop_back();
        str_.push_back(')');
        return;
      }
    }
    if(scope_.size() > 0)
      str_.push_back(c);
  }

  inline const char* str(size_t depth = 1) const noexcept {
    size_t s = scope_.size();
    if(s >= depth)
      s -= depth;
    return str_.c_str() + scope_[s];
  }
};
}
