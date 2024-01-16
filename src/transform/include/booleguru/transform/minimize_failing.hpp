#pragma once

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::transform {
/// Forks the process and reduces until the child process no longer crashes. It
/// returns an empty expression and prints the last failing expression to
/// stderr.
///
/// Should use the mutator and mutate as long as possible and save the minimal
/// variant.
struct minimize_failing {
  minimize_failing();
  virtual ~minimize_failing();

  expression::op_ref operator()(expression::op_ref o);
};
}
