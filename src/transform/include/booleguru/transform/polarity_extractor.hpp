#pragma once

namespace booleguru::expression {
class op_ref;
}

namespace booleguru::transform {
/** Sets user_flag_4 (seen in positive polarity) and user_flag_5
    (seen in negative polarity).

    Also sets the mark bit for all visited nodes.
*/
class polarity_extractor {
  public:
  polarity_extractor();
  ~polarity_extractor();

  /// Reset mark, user_flag_4, and user_flag_5 to false. This is the
  /// invariant that is needed for the result to make sense.
  void reset_user_4_5_mark(expression::op_ref o);

  expression::op_ref operator()(expression::op_ref o);
};
}
