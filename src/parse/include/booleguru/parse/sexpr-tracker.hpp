#pragma once

#include <cassert>
#include <limits>
#include <stack>
#include <string>

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
  std::stack<size_t> scope_;
  size_t stopped_at_ = 0;
  size_t last_popped_scope_ = 0;

  public:
  sexpr_tracker() = default;
  ~sexpr_tracker() = default;

  inline bool append(char c) {
    switch(c) {
      case '(':
        if(scope_.empty()) {
          str_.clear();
        }
        scope_.push(str_.size());
        str_.push_back('(');
        return stopped();
      case ')': {
        if(scope_.empty()) {
          return false;
        }
        const size_t last_size = scope_.size();
        last_popped_scope_ = scope_.top();
        scope_.pop();
        if(last_size == stopped_at_) {
          str_.push_back(c);
          return false;
        }
        break;
      }
    }
    str_.push_back(c);
    return stopped();
  }

  /** @brief Parsing has stopped.
   *
   * Returns true if the parser should pass all following chars to the
   * sexp_tracker so that some code may be executed. The parser shall then
   * append() everything while append() returns true.
   */
  inline bool stop() noexcept {
    stopped_at_ = scope_.size();
    return !scope_.empty();
  }

  inline bool stopped() const noexcept { return stopped_at_ != 0; }
  inline void stop_handled() noexcept { stopped_at_ = 0; }

  inline const char* str() const noexcept {
    return str_.c_str() + last_popped_scope_;
  }
};
}
