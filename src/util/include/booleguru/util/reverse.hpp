#pragma once

#include <iterator>

namespace booleguru::util {
// Taken from https://stackoverflow.com/a/21510202
template<typename It>
class Range {
  It b, e;

  public:
  Range(It b, It e)
    : b(b)
    , e(e) {}
  It begin() const { return b; }
  It end() const { return e; }
};

template<typename ORange,
         typename OIt = decltype(std::begin(std::declval<ORange>())),
         typename It = std::reverse_iterator<OIt>>
Range<It>
reverse(ORange&& originalRange) {
  return Range<It>(It(std::end(originalRange)), It(std::begin(originalRange)));
}
}
