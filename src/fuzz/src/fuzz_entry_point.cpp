#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include "fmt/core.h"

#include <booleguru/lua/lua-context.hpp>
#include <booleguru/parse/cli.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

using namespace booleguru;
using namespace booleguru::expression;
using namespace booleguru::lua;

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t* data_ptr, size_t size) {
  static_assert(sizeof(uint8_t) == sizeof(char));

  std::shared_ptr<op_manager> ops = std::make_shared<op_manager>();
  std::shared_ptr<lua_context> lua = std::make_shared<lua_context>(ops);

  const char* env_args = std::getenv("BOOLEGURU_ARGS");
  if(env_args) {
    // The args have to contain at least one fuzz input file!
    isviewstream view(env_args);

    booleguru::parse::cli cli(view, ops->vars_ptr(), ops, lua);
    size_t fuzz_files = cli.fuzz_files();
    if(fuzz_files == 0) {
      fmt::println("No fuzz file in $BOOLEGURU_ARGS");
      return 1;
    }

  } else {
    // Directly fuzz the CLI input, i.e. the full range of Booleguru. Also has
    // access to Lua.

    std::string_view data(reinterpret_cast<const char*>(data_ptr), size);
    isviewstream view(data);
    booleguru::parse::cli cli(view, ops->vars_ptr(), ops, lua);

    auto res = cli();
  }

  return 0;
}
