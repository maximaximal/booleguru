#include <booleguru/py/pyop.hpp>
#include <booleguru/py/python-context.hpp>

#include <pybind11/embed.h>
#include <pybind11/eval.h>

namespace booleguru::py {

python_result
eval(std::istream& in) {
  pybind11::scoped_interpreter guard{};
  try {
    auto locals = pybind11::dict();

    // The following lines add everything from the 'pybooleguru' module to the
    // __builtin__ module's namespace. Found thanks to GPT4.
    pybind11::exec("import sys\n"
                   "import pybooleguru\n"
                   "import builtins\n"
                   "mod = sys.modules['pybooleguru']\n"
                   "for name, func in mod.__dict__.items():\n"
                   "    if not name.startswith('_'):\n"
                   "        setattr(builtins, name, func)\n");

    std::string code(std::istreambuf_iterator<char>(in), {});
    pybind11::exec(code, pybind11::globals(), locals);

    py::pyop_ref op = locals["next_op"].cast<py::pyop_ref>();
    return op;
  } catch(pybind11::error_already_set& e) {
    return std::string("Python error:\n") + e.what();
  }
}
}
