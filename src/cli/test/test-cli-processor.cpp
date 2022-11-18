#include <catch2/catch_test_macros.hpp>

#include <booleguru/cli/cli-processor.hpp>
#include <booleguru/expression/op_manager.hpp>

using namespace booleguru::cli;

TEST_CASE("Parse some CLI parameters and expect a missing input file") {
  std::vector<std::string_view> args{ "--eval --not --smtlib2" };

  cli_processor cli_proc(args);
  REQUIRE_THROWS_AS(cli_proc.process(), no_input_file);
}
