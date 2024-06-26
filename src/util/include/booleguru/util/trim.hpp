#pragma once

#include <algorithm>
#include <cctype>
#include <locale>

namespace booleguru::util {
// From https://stackoverflow.com/a/217605

// trim from start (in place)
static inline void
ltrim(std::string& s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
            return !std::isspace(ch);
          }));
}

// trim from end (in place)
static inline void
rtrim(std::string& s) {
  s.erase(std::find_if(s.rbegin(),
                       s.rend(),
                       [](unsigned char ch) { return !std::isspace(ch); })
            .base(),
          s.end());
}

// trim from both ends (in place)
static inline void
trim(std::string& s) {
  rtrim(s);
  ltrim(s);
}
}
