#include <algorithm>
#include <cassert>
#include <set>
#include <string_view>

#include <booleguru/serialize/smtlib2.hpp>
#include <booleguru/transform/hash_variables.hpp>

namespace booleguru::serialize {

void
smtlib2::operator()(expression::op_ref op) {
  expression::op_manager& mgr = op.get_mgr();

  std::set<expression::op_manager::ref> vars;
  std::set<expression::op_manager::ref> quantified_vars;
  expression::op_manager::ref root = op.get_id();
  std::stack<std::pair<expression::op_manager::ref, uint32_t>> s;
  s.emplace(std::make_pair(root, 0));

  // Collect all variables!
  bool quantifiers = false;
  while(!s.empty()) {
    auto [r, d] = s.top();
    s.pop();

    const expression::op& o = mgr.getobj(r);

    switch(o.type) {
      case op_type::Exists:
      case op_type::Forall:
        quantifiers = true;
        s.emplace(std::make_pair(o.right(), 0));
        quantified_vars.insert(o.left());
        break;
      case op_type::Not:
        s.emplace(std::make_pair(o.left(), 0));
        break;
      case op_type::Var:
        vars.insert(r);
        break;
      default:
        s.emplace(std::make_pair(o.right(), 0));
        s.emplace(std::make_pair(o.left(), 0));
        break;
    }
  }

  if(quantifiers) {
    o_ << "(set-logic BV)\n";
    o_ << "\n";
  } else {
    o_ << "(set-logic QF_BV)\n";
    o_ << "\n";
  }

  std::set<expression::op_manager::ref> unquantified_vars;
  std::set_difference(
    vars.begin(),
    vars.end(),
    quantified_vars.begin(),
    quantified_vars.end(),
    std::inserter(unquantified_vars, unquantified_vars.begin()));

  for(const auto& v : unquantified_vars) {
    o_ << "(declare-const "
       << "const_" << mgr[v].to_string() << " Bool)\n";
  }
  o_ << "(assert\n";

  s.emplace(std::make_pair(root, 0));

  while(!s.empty()) {
    auto [r, d] = s.top();
    s.pop();
    const expression::op& o = mgr.getobj(r);

    if(!o.left() && !o.right()) {
      assert(o.type == op_type::Var);
      o_ << " const_" << mgr[r].to_string();
      if(d) {
        o_ << std::string(d, ')') << "\n";
      }
    } else if(o.type == op_type::Not) {
      assert(o.left());
      assert(!o.right());
      o_ << "(not";
      s.emplace(std::make_pair(o.left(), d + 1));
    } else {
      assert(o.left());
      assert(o.right());

      switch(o.type) {
        case expression::op_type::None:
          assert(false);
          break;
        case expression::op_type::Exists:
          o_ << "(exists (("
             << "const_" << mgr[o.left()].to_string() << " Bool))\n";
          quantifiers = true;
          s.emplace(std::make_pair(o.right(), d + 1));
          break;
        case expression::op_type::Forall:
          o_ << "(forall (("
             << "const_" << mgr[o.left()].to_string() << " Bool))\n";
          quantifiers = true;
          s.emplace(std::make_pair(o.right(), d + 1));
          break;
        case expression::op_type::Equi:
          o_ << "(=";
          s.emplace(std::make_pair(o.right(), d + 1));
          s.emplace(std::make_pair(o.left(), 0));
          break;
        case expression::op_type::Impl:
          o_ << "(implies";
          s.emplace(std::make_pair(o.right(), d + 1));
          s.emplace(std::make_pair(o.left(), 0));
          break;
        case expression::op_type::Lpmi:
          o_ << "(implies";
          s.emplace(std::make_pair(o.left(), d + 1));
          s.emplace(std::make_pair(o.right(), 0));
          break;
#ifdef BINARY_TREE_BINOPS
          // Strict binary tree SMTLIB2 printing

        case expression::op_type::And:
          o_ << "(and";
          s.emplace(std::make_pair(o.left(), d + 1));
          s.emplace(std::make_pair(o.right(), 0));
          break;
        case expression::op_type::Or:
          o_ << "(or";
          s.emplace(std::make_pair(o.left(), d + 1));
          s.emplace(std::make_pair(o.right(), 0));
          break;
#else
        case expression::op_type::Or: {
          o_ << "(or";
          s.emplace(std::make_pair(o.right(), d + 1));
          auto l = o.left();
          while(mgr[l]->type == op_type::Or) {
            s.emplace(std::make_pair(mgr[l]->right(), 0));
            l = mgr[l]->left();
          }
          s.emplace(std::make_pair(l, 0));
          break;
        }
        case expression::op_type::And: {
          o_ << "(and";
          s.emplace(std::make_pair(o.right(), d + 1));
          auto l = o.left();
          while(mgr[l]->type == op_type::And) {
            s.emplace(std::make_pair(mgr[l]->right(), 0));
            l = mgr[l]->left();
          }
          s.emplace(std::make_pair(l, 0));
          break;
        }
#endif
        case expression::op_type::Xor:
          o_ << "(xor";
          s.emplace(std::make_pair(o.right(), d + 1));
          s.emplace(std::make_pair(o.left(), 0));
          break;
        case expression::op_type::Not:
          assert(false);
          break;
        case expression::op_type::Var:
          assert(false);
          break;
      }
    }
  }

  o_ << ")\n(check-sat)\n";
}
}
