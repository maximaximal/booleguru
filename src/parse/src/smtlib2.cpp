#include <booleguru/cl/ecl-wrapper.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>

#include <iostream>

namespace booleguru::parse {
smtlib2::smtlib2(int fileno,
                 std::shared_ptr<expression::op_manager> ops)
  : base(std::cin, ops->vars_ptr(), ops)
  , fileno_(fileno) {}

smtlib2::smtlib2(std::string_view str,
                 std::shared_ptr<expression::op_manager> ops)
  : base(std::cin, ops->vars_ptr(), ops)
  , str_(str) {}

result
smtlib2::operator()() {
  if(str_.empty()) {
    str_ = "(parse-smtlib2-from-fileno " + std::to_string(fileno_) + ")";
  } else {
    str_ = "(parse-smtlib2-from-str '" + str_ + "')";
  }
  auto ret = cl::ecl_wrapper::get().eval(str_.c_str(), ops_);
  if(std::holds_alternative<std::string>(ret)) {
    return error("Error during SMTLIB2 processing: " +
                 std::get<std::string>(ret));
  } else if(std::holds_alternative<expression::op_ref>(ret)) {
    return generate_result(std::get<expression::op_ref>(ret));
  }
  return error("Could not get error or op from SMTLIB!");
}
}
