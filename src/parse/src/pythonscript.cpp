#include "booleguru/py/python-context.hpp"
#include <booleguru/parse/pythonscript.hpp>

#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
result
pythonscript::operator()() {
  py::python_result eval_result = py::eval(in_);

  if(std::string* err = std::get_if<std::string>(&eval_result)) {
    return error(*err);
  } else if(expression::op_ref* op
            = std::get_if<expression::op_ref>(&eval_result)) {
    return generate_result(*op);
  } else {
    return error("Invalid return type of python script! Has to set the "
                 "returned op to `next_op`.");
  }
}
}
