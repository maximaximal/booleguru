#include <iostream>
#include <memory>
#include <string>

#include <emscripten/bind.h>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace emscripten;
using namespace booleguru;

int
execute_booleguru(std::string input, std::string type) {
  std::string_view view(input);
  auto is = isviewstream(view);
  auto boole = std::make_unique<parse::boole>(is);
  // Web is a sandbox, can always just enable eval.
  boole->eval(true);
  auto res = (*boole)();
  if(!res) {
    std::cerr << "Parse error! Result: \"" << res
              << "\", error: " << res.message << std::endl;
    return 1;
  }
  return 0;
}

EMSCRIPTEN_BINDINGS(booleguru) {
  function("execute", &execute_booleguru);
}
