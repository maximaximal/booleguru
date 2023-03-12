#include <functional>
#include <iostream>
#include <memory>
#include <string>

#include <emscripten/bind.h>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace emscripten;
using namespace booleguru;

using more_data_cb = std::function<std::string(std::string)>;
more_data_cb js_more_data_cb;

int
execute_booleguru(std::string input, std::string type, more_data_cb cb) {
  js_more_data_cb = cb;

  std::string_view view(input);
  auto is = isviewstream(view);
  try {
    auto boole = std::make_unique<parse::boole>(is);
    // Web is a sandbox, can always just enable eval.
    boole->eval(true);
    auto res = (*boole)();
    if(res) {
      std::cout << res << std::endl;
    } else {
      std::cerr << "Parse error! Result: \"" << res
                << "\", error: " << res.message << std::endl;
      return 1;
    }
    return 0;
  } catch(std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  } catch(...) {
    std::cerr << "Unknown Error!" << std::endl;
    return 1;
  }
}

EMSCRIPTEN_BINDINGS(booleguru) {
  function("execute", &execute_booleguru);
}
