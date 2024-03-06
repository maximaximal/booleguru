#include <cassert>
#include <stack>

#include <booleguru/expression/id.hpp>
#include <booleguru/expression/op.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/transform/polarity_extractor.hpp>

namespace booleguru::transform {
polarity_extractor::polarity_extractor() = default;
polarity_extractor::~polarity_extractor() = default;

void
polarity_extractor::reset_user_4_5_mark(expression::op_ref o) {
  expression::op_manager& mgr = o.get_mgr();
  mgr.traverse_depth_first_through_tree(
    o.get_id(), [](expression::op_id id, const expression::op& op) {
      (void)id;
      op.mark = false;
      op.user_flag4 = false;
      op.user_flag5 = false;
    });
}

expression::op_ref
polarity_extractor::operator()(expression::op_ref o) {
  std::stack<std::pair<expression::op_id, uint32_t>> s;
  s.push(std::make_pair(o.get_id(), 0));

  expression::op_manager& mgr = o.get_mgr();

  using enum expression::op_type;

  while(!s.empty()) {
    const auto [id, nots] = s.top();
    const expression::op& op = mgr.getobj(id);
    s.pop();

    op.user_flag4 |= nots % 2 == 0;
    op.user_flag5 |= nots % 2 == 1;
    op.mark = true;

    switch(op.type) {
      case None:
        assert(false);
        break;
      case Exists:
        [[fallthrough]];
      case Forall:
        [[fallthrough]];
      case Equi:
        [[fallthrough]];
      case Impl:
        [[fallthrough]];
      case Lpmi:
        [[fallthrough]];
      case Or:
        [[fallthrough]];
      case And:
        [[fallthrough]];
      case Xor:
        s.emplace(std::make_pair(op.left(), nots));
        s.emplace(std::make_pair(op.right(), nots));
        break;
      case Not:
        s.emplace(std::make_pair(op.left(), nots + 1));
        break;
      case Var:
        break;
    }
  }

  return o;
}
}
