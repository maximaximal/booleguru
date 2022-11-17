#include <booleguru/cli/argument.hpp>
#include <booleguru/cli/cli-processor.hpp>
#include <booleguru/cli/input_file.hpp>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>

#include <cassert>
#include <string>
#include <unordered_map>

#define CUR_IS(OP)                              \
  arg_op* o = std::get_if<arg_op>(&cur_.get()); \
  *o == OP

namespace booleguru::cli {

static std::unordered_map<std::string_view, cli_processor::arg_op>
  arg_op_strings = { { "and", cli_processor::And },
                     { "or", cli_processor::Or },
                     { "not", cli_processor::Not },
                     { "implies", cli_processor::Implies },
                     { "equivalent", cli_processor::Equivalent },
                     { "xor", cli_processor::Xor },
                     { "lpar", cli_processor::LPar },
                     { "rpar", cli_processor::RPar } };

static expression::op_type arg_op_to_op[] = {
  expression::op_type::And,  expression::op_type::Or,
  expression::op_type::Not,  expression::op_type::Impl,
  expression::op_type::Equi, expression::op_type::Xor,
};

cli_processor::cli_processor(arg_vec args)
  : args_(process_args_to_inputs(args))
  , cur_(args_[0])
  , next_(args_[1])
  , ops_(std::make_shared<expression::op_manager>()) {}

template<cli_processor::arg_op type, typename Functor>
expression::op_ref
cli_processor::process_assoc_op(Functor next) {
  expression::op_ref res;
  bool done = false;
  do {
    expression::op_ref child = next();
    res = res.valid() ? ops_->get(expression::op(
                          arg_op_to_op[type], res.get_id(), child.get_id()))
                      : child;
    if(CUR_IS(type)) {
      this->next();
    } else {
      done = true;
    }
  } while(res.valid() && !done);

  return res;
}

expression::op_ref
cli_processor::process_equivalence() {
  return process_assoc_op<Equivalent>([this]() { return process_implies(); });
}
expression::op_ref
cli_processor::process_implies() {
  return process_assoc_op<Implies>([this]() { return process_xor(); });
}
expression::op_ref
cli_processor::process_xor() {
  return process_assoc_op<Xor>([this]() { return process_or(); });
}
expression::op_ref
cli_processor::process_or() {
  return process_assoc_op<Or>([this]() { return process_and(); });
}
expression::op_ref
cli_processor::process_and() {
  return process_assoc_op<And>([this]() { return process_not(); });
}
expression::op_ref
cli_processor::process_not() {
  if(CUR_IS(Not)) {
    next();
    auto child = process_not();
    return ops_->get(
      expression::op(expression::op_type::Not, child.get_id(), 0));
  } else {
    return process_basic();
  }
}
expression::op_ref
cli_processor::process_basic() {
  if(CUR_IS(LPar)) {
    next();
    auto child = process_expr();
    if(CUR_IS(RPar)) {
      return child;
    } else {
      throw std::invalid_argument("No --rpar after an initiating --lpar.");
    }
  }

  // Now we finally are at the most basic level. Just get the op from the
  // current thing.
  if(cur_.get().index() != 1) {
    throw no_input_file("Expected some input file");
  }

  return process_input_file(std::get<arg_vec>(cur_.get()));
}
expression::op_ref
cli_processor::process_expr() {
  return process_equivalence();
}
cli_processor::arg_stream
cli_processor::process_args_to_inputs(arg_vec& args) const {
  arg_stream inputs;

  arg_vec curr;
  for(auto it = args.begin(); it != args.end(); ++it) {
    const auto& arg = *it;

    assert(arg.size() > 0);

    if(arg.size() >= 2 && arg.at(0) == '-' && arg.at(1) == '-') {
      // Check for operation on input.
      auto opit = arg_op_strings.find(arg.substr(2));
      if(opit != arg_op_strings.end()) {
        inputs.emplace_back(opit->second);
        continue;
      } else {
        // Otherwise it was just a normal argument to be passed.
        curr.emplace_back(arg);
        continue;
      }
    }
    curr.emplace_back(arg);
    if(arg.at(0) != '-' || arg.length() == 1) {
      // This is a file! Ends curr.
      inputs.emplace_back(curr);
    }
  }
  return inputs;
}

expression::op_ref
cli_processor::process_input_file(const arg_vec& v) {
  assert(v.size() >= 1);

  std::vector<argument> arguments;
  for(auto it = v.begin(); it != v.begin() + v.size() - 1; ++it) {
    const auto& arg = *it;
    if(arg.size() > 1 && arg.at(0) == '-' && arg.at(1) == '-') {
      std::string_view a1 = arg;
      auto next = it + 1;
      if(next != v.end() && next->size() > 0 && next->at(0) != '-') {
        arguments.emplace_back(a1, *next);
      } else {
        arguments.emplace_back(a1);
      }
    }
  }
  std::string_view path = v[v.size() - 1];
  input_file f(path, std::move(arguments));
  return f.process();
}

void
cli_processor::next() {
  ++cur_idx_;
  cur_ = next_;
  if(cur_idx_ + 1 >= args_.size()) {
    throw std::out_of_range(
      "Trying to call next() but no input parameters are left.");
  }
  next_ = args_[cur_idx_ + 1];
}
}
