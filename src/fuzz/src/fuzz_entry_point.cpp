#include "booleguru/util/is_number.hpp"
#include "booleguru/util/unsupported.hpp"
#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include <fmt/core.h>

#include <antlr4-runtime.h>

#include <booleguru/lua/lua-context.hpp>
#include <booleguru/parse/cli.hpp>
#include <booleguru/parse/error.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/util/istringviewstream.hpp>

#include <booleguru/fuzz/source.hpp>

using namespace booleguru;
using namespace booleguru::expression;
using namespace booleguru::lua;

static bool
check_op_invariants(op_manager& ops, op& o) {
  if(o.type > op_type::Var) {
    return false;
  }
  if(o.is_quant() || o.is_binop()) {
    if(o.left().id_ == 0 || o.left().id_ > ops.size()) {
      return false;
    }
    if(o.right().id_ == 0 || o.right().id_ > ops.size()) {
      return false;
    }
  }
  if(o.type == op_type::Var) {
    // Don't care about the var manager, variables are just IDs.
    return true;
  }
  if(o.is_unop()) {
    if(o.left().id_ == 0 || o.left().id_ > ops.size()) {
      return false;
    }
  }
  return true;
}

extern "C" size_t
LLVMFuzzerMutate(uint8_t* data, size_t size, size_t max_size);

extern "C" size_t
LLVMFuzzerCustomMutator(uint8_t* data,
                        size_t size,
                        size_t max_size,
                        unsigned int seed) {
  static bool env_checked = false;
  static bool ops_fuzzing = false;
  if(!env_checked) {
    const char* env_args = std::getenv("BOOLEGURU_ARGS");
    if(env_args)
      ops_fuzzing = true;
    env_checked = true;
  }

  if(ops_fuzzing) {
    // TODO Use our custom expression mutator.
  } else {
    return LLVMFuzzerMutate(data, size, max_size);
  }
}

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t* data_ptr, size_t size) {
  static_assert(sizeof(uint8_t) == sizeof(char));

  std::shared_ptr<op_manager> ops = std::make_shared<op_manager>();
  std::shared_ptr<lua_context> lua = std::make_shared<lua_context>(ops);

  const char* env_args = std::getenv("BOOLEGURU_ARGS");

  try {
    if(env_args) {
      // The args have to contain at least one fuzz input file!
      isviewstream view_(env_args);

      booleguru::parse::cli cli_(view_, ops->vars_ptr(), ops, lua);
      size_t fuzz_files = cli_.fuzz_files();
      if(fuzz_files == 0) {
        fmt::println("No fuzz file in $BOOLEGURU_ARGS");
        assert(false);
        return -1;
      }

      booleguru::fuzz::source s(*ops);
      size_t inserted = s.apply(data_ptr, size);

      // There have to be at least enough ops to satisfy the input fuzz files.
      if(fuzz_files > ops->size()) {
        return -1;
      }

      isviewstream view(env_args);
      booleguru::parse::cli cli(view, ops->vars_ptr(), ops, lua);

      size_t fuzz_file_counter = 0;

      cli.parse_file_using([ops, fuzz_files, &fuzz_file_counter](
                             std::string_view v, util::type type) -> op_ref {
        // Just return different sections of the op tree.
        if(v == "fuzz") {
          size_t ops_size = ops->size();
          uint32_t opid = ops_size - (fuzz_files - fuzz_file_counter++) + 1;
          return (*ops)[opid];
        } else {
          // No input file handling for fuzzers.
          return ops->top();
        }
      });

      auto res = cli();

      /*
      for(size_t i = 1; i <= ops->size(); ++i) {
        const op& o_ = ops->getobj(i);
        fmt::println("Op {} is type {}, left: {}, right: {}",
                     i,
                     (int)o_.type,
                     o_.left().id_,
                     o_.right().id_);
    }
      */
    } else {
      // Directly fuzz the CLI input, i.e. the full range of Booleguru. Also has
      // access to Lua.

      std::string_view data(reinterpret_cast<const char*>(data_ptr), size);
      isviewstream view(data);
      booleguru::parse::cli cli(view, ops->vars_ptr(), ops, lua);

      cli.parse_file_using(
        [ops](std::string_view v, util::type type) { return ops->top(); });

      auto res = cli();
      if(!res) {
        // If there is no valid result, also don't add it to the corpus.
        return -1;
      }
    }
  } catch(antlr4::IllegalArgumentException& e) {
    // This is invalid UTF-8.
    return -1;
  } catch(booleguru::lua::fennel_error& e) {
    // It is not very interesting to find invalid fennel calls, as this is very
    // easy.
    return -1;
  } catch(booleguru::util::not_a_number& e) {
    // Generating inputs with parts that are not numbers is also really easy.
    return -1;
  } catch(booleguru::parse::error::generic& e) {
    // Any generic parse errors are also not interesting.
    return -1;
  } catch(booleguru::util::unsupported& e) {
    // If something is unsupported, it is not an error, it is a known issue.
    return -1;
  }

  return 0;
}
