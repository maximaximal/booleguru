#include <cassert>
#include <string_view>
#include <unordered_set>

#include <booleguru/serialize/smtlib2.hpp>

namespace booleguru::serialize {

void
smtlib2::operator()(expression::op_ref op) {
  expression::op_manager& mgr = op.get_mgr();

  std::stack<std::string> expr;

  std::unordered_set<std::string> vars;

  bool quantifiers = false;

  expression::op_manager::ref root = op.get_id();
  mgr.traverse_postorder_with_stack(
    root,
    [&vars, &expr, &quantifiers](expression::op_manager* mgr,
                                 expression::op_manager::ref r) -> void {
      const expression::op& o = mgr->getobj(r);

      if(!o.left() && !o.right()) {
        assert(o.type == op_type::Var);
        std::string v = "const_" + (*mgr)[r].to_string();
        expr.emplace(v);
        vars.emplace(v);
      } else if(o.type == op_type::Not) {
        assert(o.left());
        assert(!o.right());
        std::string ex = std::move(expr.top());
        expr.pop();
        expr.emplace("(not " + ex + ")");
      } else {
        assert(o.left());
        assert(o.right());

        std::string right = std::move(expr.top());
        expr.pop();
        std::string left = std::move(expr.top());
        expr.pop();

        std::string e;

        switch(o.type) {
          case expression::op_type::None:
            assert(false);
            break;
          case expression::op_type::Exists:
            e = "(exists ((" + left + " Bool)) " + right + ")";
            quantifiers = true;
            break;
          case expression::op_type::Forall:
            e = "(forall ((" + left + " Bool)) " + right + ")";
            quantifiers = true;
            break;
          case expression::op_type::Equi:
            e = "(= " + left + " " + right + ")";
            break;
          case expression::op_type::Impl:
            e = "(implies " + left + " " + right + ")";
            break;
          case expression::op_type::Lpmi:
            e = "(implies " + right + " " + left + ")";
            break;
          case expression::op_type::Or:
            e = "(or " + left + " " + right + ")";
            break;
          case expression::op_type::And:
            e = "(and " + left + " " + right + ")";
            break;
          case expression::op_type::Xor:
            e = "(xor " + left + " " + right + ")";
            break;
          case expression::op_type::Not:
            assert(false);
            break;
          case expression::op_type::Var:
            assert(false);
            break;
        }
        expr.emplace(std::move(e));
      }
    });
  assert(expr.size() == 1);

  if(quantifiers) {
    o_ << "(set-logic BV)\n";
    o_ << "\n";
  } else {
    o_ << "(set-logic QF_BV)\n";
    o_ << "\n";
  }
  for(const auto& v : vars) {
    o_ << "(declare-const " << v << " Bool)\n";
  }
  o_ << "\n";
  o_ << "(assert " << expr.top() << ")\n";
  o_ << "(check-sat)\n";
}
}
