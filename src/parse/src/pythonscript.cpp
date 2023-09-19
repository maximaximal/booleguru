#include "booleguru/lua/binding-helpers.hpp"
#include "booleguru/py/python-context.hpp"

#include <booleguru/expression/literals.hpp>

#include <booleguru/parse/pythonscript.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
result
pythonscript::operator()() {
  // Always make sure the global is exactly right
  expression::literals::handle::global(ops_);
  assert(&expression::literals::handle::global().get_op_manager()
         == ops_.get());

  py::python_result eval_result = py::eval(in_);

  assert(&expression::literals::handle::global().get_op_manager()
         == ops_.get());

  if(std::string* err = std::get_if<std::string>(&eval_result)) {
    return error(*err);
  } else if(expression::op_ref* op
            = std::get_if<expression::op_ref>(&eval_result)) {
    assert(&op->get_mgr() == ops_.get());
    return generate_result(*op);
  } else {
    return error("Invalid return type of python script! Has to set the "
                 "returned op to `next_op`.");
  }
}
}
