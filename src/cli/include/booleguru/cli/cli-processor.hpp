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

namespace booleguru::lua {
class lua_context;
}

struct no_input_file : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct fennel_error : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct fennel_invalid_return_type : public std::invalid_argument {
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

  using arg_variant = std::variant<arg_op, arg_vec, std::string_view>;
  using arg_stream = std::vector<arg_variant>;

  explicit cli_processor(arg_vec args);
  explicit cli_processor(int argc, char* argv[])
    : cli_processor(arg_vec(argv + 1, argv + argc)) {}

  expression::op_ref process();

  const argument::param_variant& output_arg(argument::keywords k);

  expression::op_ref consume_eventual_lisp_arguments(
    expression::op_ref last_op);

  private:
  argument::param_variant output_args_[argument::keywords::count_] = {
    argument::boole, /* type */
    false            /* eval */
  };

  arg_stream process_args_to_inputs(arg_vec& args);

  expression::op_ref process_input_file(const arg_vec& v);
  expression::op_ref process_input_fennel(arg_vec& v);

  template<cli_processor::arg_op type, typename Functor>
  expression::op_ref process_assoc_op(Functor next);

  arg_vec input_args_;
  size_t cur_idx_ = 0;
  arg_stream args_;
  std::reference_wrapper<arg_variant> cur_;
  std::reference_wrapper<arg_variant> next_;
  std::shared_ptr<expression::op_manager> ops_;
  std::shared_ptr<lua::lua_context> lua_;
  void next();
};
}
