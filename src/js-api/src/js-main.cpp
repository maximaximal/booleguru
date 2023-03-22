#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <string>

#include <emscripten/bind.h>

#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace emscripten;
using namespace booleguru;

std::string
getExceptionMessage(intptr_t exceptionPtr) {
  return std::string(reinterpret_cast<std::exception*>(exceptionPtr)->what());
}

EMSCRIPTEN_BINDINGS(Bindings) {
  emscripten::function("getExceptionMessage", &getExceptionMessage);
};

using more_data_cb = std::function<std::string_view(std::string)>;
more_data_cb js_more_data_cb;

int
execute_booleguru(std::string input,
                  std::string type,
                  std::map<std::string, std::string> more_code) {
  js_more_data_cb = [&more_code](std::string name) -> std::string_view {
    auto it = more_code.find(name);
    if(it != more_code.end()) {
      return it->second;
    }
    return "";
  };

  std::string_view view(input);
  auto is = isviewstream(view);
  try {
    auto boole = std::make_unique<parse::boole>(is);
    // Web is a sandbox, can always just enable eval.
    boole->eval(true);
    auto res = (*boole)();
    if(res) {
      std::cout << *res << std::endl;
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
  register_map<std::string, std::string>("StringMap");
}
