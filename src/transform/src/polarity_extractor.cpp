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
  for(auto& op : o.get_mgr().objects()) {
    op.first.mark = false;
    op.first.user_flag4 = false;
    op.first.user_flag5 = false;
  }
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
      case Equi:
        [[fallthrough]];
      case Xor:
        s.emplace(std::make_pair(op.left(), nots + 1));
        s.emplace(std::make_pair(op.right(), nots + 1));
	break;
      case Impl:
        s.emplace(std::make_pair(op.left(), nots + 1));
        s.emplace(std::make_pair(op.right(), nots));
	break;
      case Lpmi:
        s.emplace(std::make_pair(op.left(), nots));
        s.emplace(std::make_pair(op.right(), nots + 1));
	break;
      case Exists:
        [[fallthrough]];
      case Forall:
        [[fallthrough]];
      case Or:
        [[fallthrough]];
      case And:
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
