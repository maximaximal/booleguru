#pragma once

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::transform {
expression::op_ref
distribute_to_cnf(expression::op_ref r);
}
