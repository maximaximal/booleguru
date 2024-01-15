#pragma once

#include <string>

namespace booleguru::util {
// Taken from https://stackoverflow.com/a/14678964
inline void
str_replace(std::string& subject,
            const std::string& search,
            const std::string& replace) {
  size_t pos = 0;
  while((pos = subject.find(search, pos)) != std::string::npos) {
    subject.replace(pos, search.length(), replace);
    pos += replace.length();
  }
}

inline bool
str_replace_first_rest(std::string& subject,
                       const std::string& search,
                       const std::string& replace_first,
                       const std::string& replace_rest) {
  bool first = true;
  size_t pos = 0;
  while((pos = subject.find(search, pos)) != std::string::npos) {
    if(first) {
      subject.replace(pos, search.length(), replace_first);
      pos += replace_first.length();
      first = false;
    } else {
      subject.replace(pos, search.length(), replace_rest);
      pos += replace_rest.length();
    }
  }
  return !first;
}
}
