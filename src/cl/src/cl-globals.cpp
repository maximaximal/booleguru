#include <booleguru/cl/cl-globals.hpp>

namespace booleguru::cl {
cl_object clfun_eval;
cl_object cltype_variable;
cl_object cltype_op;
booleguru::expression::op_manager* op_manager = nullptr;
}
