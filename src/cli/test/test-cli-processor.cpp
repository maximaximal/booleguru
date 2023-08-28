#include <catch2/catch_test_macros.hpp>

#include <booleguru/cli/cli-processor.hpp>
#include <booleguru/expression/op_manager.hpp>

using namespace booleguru::cli;

TEST_CASE("Parse some CLI parameters and expect a missing input file") {
  int argc = 1;
  const char* argv[] = { "booleguru", "hello" };
  cli_processor cli_proc(argc, argv);
}
