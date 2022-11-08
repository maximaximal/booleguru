#pragma once

typedef union cl_lispunion* cl_object;

namespace booleguru::expression {
class op_manager;
}

namespace booleguru::cl {
extern cl_object clfun_eval;
extern cl_object clfun_b_make_op;
extern cl_object cltype_variable;
extern cl_object cltype_op;
extern booleguru::expression::op_manager* op_manager;
}
