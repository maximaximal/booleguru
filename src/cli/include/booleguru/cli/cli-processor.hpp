#pragma once

#include <memory>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "argument.hpp"

namespace booleguru::expression {
class op_ref;
class op_manager;
}

struct no_input_file : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

namespace booleguru::cli {
class cli_processor {
  // Logical Parsing of command line input.
  expression::op_ref process_equivalence();
  expression::op_ref process_implies();
  expression::op_ref process_or();
  expression::op_ref process_and();
  expression::op_ref process_xor();
  expression::op_ref process_not();
  expression::op_ref process_basic();
  expression::op_ref process_expr();

  public:
  using arg_vec = std::vector<std::string_view>;

  enum arg_op {
    And,
    Or,
    Not,
    Implies,
    Equivalent,
    Xor,
    LPar,
    RPar,
    None,
  };

  using arg_variant = std::variant<arg_op, arg_vec>;
  using arg_stream = std::vector<arg_variant>;

  explicit cli_processor(arg_vec args);
  explicit cli_processor(int argc, char* argv[])
    : cli_processor(arg_vec(argv + 1, argv + argc)) {}

  expression::op_ref process();

  const argument::param_variant& output_arg(argument::keywords k);

  private:

  argument::param_variant output_args_[argument::keywords::count_] = {
    argument::boole, /* type */
    false            /* eval */
  };

  arg_stream process_args_to_inputs(arg_vec& args);

  expression::op_ref process_input_file(const arg_vec& v);

  template<cli_processor::arg_op type, typename Functor>
  expression::op_ref process_assoc_op(Functor next);

  size_t cur_idx_ = 0;
  arg_stream args_;
  std::reference_wrapper<arg_variant> cur_;
  std::reference_wrapper<arg_variant> next_;
  std::shared_ptr<expression::op_manager> ops_;
  void next();
};
}
