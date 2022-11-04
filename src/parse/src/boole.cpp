#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/cl/ecl-wrapper.hpp>

#include <iostream>

namespace booleguru::parse {
using namespace expression;

boole::token::token() {}
boole::token::~token() {}

const char* token_type_str[] = { "Ident",  "LPar",        "RPar",    "Exists",
                                 "Forall", "Equivalence", "Implies", "Seilpmi",
                                 "Or",     "And",         "Not",     "None" };
const char* token_type_sym[] = { "_",  "(",  ")", "?", "@", "<->",
                                 "->", "<-", "|", "&", "!", "." };

boole::token&
boole::token::operator=(token&& o) {
  if(o.type == Ident) {
    ident = std::move(o.ident);
  } else
    ident.clear();

  type = o.type;
  line = o.line;
  col = o.col;

  return *this;
}

boole::token::token_type
boole::token_type_from_op_type(expression::op_type t) {
  switch(t) {
    case op_type::And:
      return token::And;
    case op_type::Impl:
      return token::Implies;
    case op_type::Equi:
      return token::Equivalence;
    case op_type::Lpmi:
      return token::Seilpmi;
    case op_type::Or:
      return token::Or;
    default:
      return token::None;
  }
}

bool
boole::next() {
  cur_ = std::move(next_);
  next_.ident.clear();

  next_.type = token::None;

  auto& type = next_.type;
  auto& ident = next_.ident;

  for(;;) {
    if(c_processed_) {
      in_ >> c_;
      c_processed_ = false;
      c_appended_ = false;

      std::cout << "Cur: " << cur_ << ", Parsed " << c_
                << ", stopped: " << sexp_.stopped() << std::endl;

      if(c_ == EOF || in_.eof()) {
        comment_ = false;
        c_ = '\0';
      }
    }

    if(c_ == '\n') {
      next_.col = 0;
      ++next_.line;
    } else {
      ++next_.col;
    }

    if(comment_) {
      c_processed_ = true;
      if(c_ == '\n') {
        comment_ = false;
      }
      continue;
    }

    if(!c_appended_) {
      c_appended_ = true;
      std::cout << "append: " << c_ << " stopped: " << sexp_.stopped()
                << std::endl;
      if(sexp_.append(c_)) {
        c_processed_ = true;
        continue;
      } else if(sexp_.stopped()) {
        c_processed_ = true;
        sexp_.stop_handled();
        next_.type = token::RPar;
        return true;
      }
    }

    auto ident_end_detect = [this, &type, &ident]() -> bool {
      // Unicode characters over and no ident was given!
      if(!ident.empty()) {
        type = token::Ident;
        // _c remains unprocessed! The next iteration covers it.
        return true;
      }
      return false;
    };

    switch(c_) {
      case '<':
        if(equivalent_ || seilpmi_) {
          return error("Invalid construction of <- or <->!");
        }
        equivalent_ = true;
        seilpmi_ = true;
        c_processed_ = true;
        continue;
      case '-':
        if(middledash_) {
          return error("Invalid construction of <-, ->, or <->!");
        }
        if(!seilpmi_ && !equivalent_)
          implies_ = true;
        c_processed_ = true;
        middledash_ = true;
        continue;
      case '>':
        if(equivalent_ && middledash_) {
          if(ident_end_detect())
            return true;
          c_processed_ = true;
          type = token::Equivalence;
          middledash_ = false;
          equivalent_ = false;
          return true;
        } else if(implies_ && middledash_) {
          if(ident_end_detect())
            return true;
          c_processed_ = true;
          type = token::Implies;
          middledash_ = false;
          implies_ = false;
          return true;
        } else {
          return error("Invalid construction of -> or <->!");
        }
        break;

      case '\xE2':
        if(!unicode_op_prefix1_)
          unicode_op_prefix1_ = true, c_processed_ = true;
        else
          return error("Invalid construction of unicode op! (Prefix 1)");
        break;
      case '\x88':
        if(unicode_op_prefix1_ && !unicode_op_prefix2_)
          unicode_op_prefix2_ = true, c_processed_ = true;
        else
          return error("Invalid construction of unicode op! (Prefix 2)");
        break;

      case '\x80':// Forall
        if(unicode_op_prefix2_)
          c_ = '@', unicode_op_prefix2_ = false, ident.pop_back(),
          ident.pop_back();
        break;
      case '\x83':// Exists
        if(unicode_op_prefix2_)
          c_ = '?', unicode_op_prefix2_ = false, ident.pop_back(),
          ident.pop_back();
        break;
      case '\xA7':// And
        if(unicode_op_prefix2_)
          c_ = '&', unicode_op_prefix2_ = false, ident.pop_back(),
          ident.pop_back();
        break;
      case '\xA8':// Or
        if(unicode_op_prefix2_)
          c_ = '|', unicode_op_prefix2_ = false, ident.pop_back(),
          ident.pop_back();
        break;

      default:
        c_processed_ = false;
        if(seilpmi_ && middledash_) {
          if(ident_end_detect())
            return true;
          type = token::Seilpmi;
          seilpmi_ = false;
          equivalent_ = false;
          middledash_ = false;
          return true;
        }
        seilpmi_ = false;
        equivalent_ = false;
        middledash_ = false;
        unicode_op_prefix1_ = false;
        unicode_op_prefix2_ = false;
        break;
    }

    if(c_ & 0b1000'0000) {
      // Unicode multibyte detected! Extend current ident.
      ident.push_back(c_);
      c_processed_ = true;
      continue;
    } else {
      // Some regular old ident characters.
      if(std::isalnum(c_) || c_ == '_') {
        ident.push_back(c_);
        c_processed_ = true;
        continue;
      }

      if(ident_end_detect())
        return true;

      // All other processes swallow the current character.
      c_processed_ = true;

      if(c_ == '\0')
        return false;

      // Spaces should be ignored.
      if(isspace(c_)) {
        continue;
      }

      switch(c_) {
        case '(':
          type = token::LPar;
          break;
        case ')':
          type = token::RPar;
          break;
        case '|':
        case '/':
          type = token::Or;
          break;
        case '&':
          type = token::And;
          break;
        case '?':
          type = token::Exists;
          break;
        case '!':
        case '~':
          type = token::Not;
          break;
        case '@':
        case '#':
          type = token::Forall;
          break;
        case '%':
          comment_ = true;
          continue;
        default:
          // Lisp execution means that everything that cannot be understood is
          // an ident.
          type = token::Ident;
      }

      return true;
    }
  }
}

template<expression::op_type type, typename Functor>
result
boole::parse_assoc_op(Functor next) {
  result res;
  bool done = false;
  do {
    result child = next();
    if(child) {
      res =
        res
          ? generate_result(ops_->get(op(type, res->get_id(), child->get_id())))
          : child;
      if(cur_.type == token_type_from_op_type(type)) {
        this->next();
      } else {
        done = true;
      }
    } else {
      return child;
    }
  } while(res && !done);

  return res;
}

result
boole::parse_iff() {
  return parse_assoc_op<op_type::Equi>([this]() { return parse_implies(); });
}
result
boole::parse_implies() {
  return parse_assoc_op<op_type::Impl>([this]() { return parse_seilpmi(); });
}
result
boole::parse_seilpmi() {
  return parse_assoc_op<op_type::Lpmi>([this]() { return parse_or(); });
}
result
boole::parse_or() {
  return parse_assoc_op<op_type::Or>([this]() { return parse_and(); });
}
result
boole::parse_and() {
  return parse_assoc_op<op_type::And>([this]() { return parse_not(); });
}

result
boole::parse_not() {
  if(cur_.type == token::Not) {
    next();
    result child = parse_not();
    if(child) {
      return generate_result(ops_->get(op(op_type::Not, child->get_id(), 0)));
    } else {
      return child;
    }
  } else {
    return parse_basic();
  }
}

result
boole::parse_basic() {
  if(cur_.type == token::LPar) {
    next();
    if(next_.type == token::RPar) {
      // This is a lisp expression, not a boolean expression! Does not matter
      // what cur_ is, as this is just (something).

      // Stop at the ident in-between.
      sexp_.stop();
      // Continue to the RPar.
      next();
      // Advance over the RPar.
      next();

      cl::ecl_wrapper& ecl = cl::ecl_wrapper::get();
      auto ret = ecl.eval(sexp_.str(), ops_);
      if(std::holds_alternative<std::string>(ret)) {
        return error("Error occurred during non-interactive lisp execution: " +
                     std::get<std::string>(ret));
      }
      if(std::holds_alternative<op_ref>(ret)) {
        return generate_result(std::get<op_ref>(ret));
      }
      return generate_result(op_ref());
    } else {
      // Okay, this is not just some (variable), but actually more content.
      // Could be a logical expression.
      result child = parse_expr();
      if(!child) {
        return error("No child parsed in parse_basic(), because: " +
                     child.message);
      }
      if(cur_.type != token::RPar) {
        return error("Expected )");
      }
      next();
      return child;
    }
  } else if(cur_.type == token::Ident) {
    auto varref = vars_->get(variable{ std::move(cur_.ident) });
    next();
    auto v = ops_->get(op(op_type::Var, varref.get_id(), 0));
    return generate_result(v);
  } else {
    return error("Expected ident in parse_basic");
  }

  return error("No other parsing in parse_basic!");
}

result
boole::parse_expr() {
  if((cur_.type == token::Exists || cur_.type == token::Forall)) {
    op_type quanttype;
    if(cur_.type == token::Exists)
      quanttype = op_type::Exists;
    if(cur_.type == token::Forall)
      quanttype = op_type::Forall;
    if(!next())
      return error("Expected ident, got no new token.");

    if(cur_.type != token::Ident) {
      return error("Expected ident");
    }
    auto varref = vars_->get(variable{ std::move(cur_.ident) });
    if(!next())
      return error("Expected sub-expression, got no new token");
    auto child = parse_expr();
    if(child)
      return generate_result(
        ops_->get(op(quanttype, child->get_id(), varref.get_id())));
    else
      return error("Expected some sub-expression");
  }

  result iff = parse_iff();
  if(!iff) {
    // Expression could not be parsed! Maybe stop parsing and try lisp.
    assert(!sexp_.stopped());
    sexp_.stop();
    next();

    cl::ecl_wrapper& ecl = cl::ecl_wrapper::get();
    auto ret = ecl.eval(sexp_.str(), ops_);
    if(std::holds_alternative<std::string>(ret)) {
      return error("Error occurred during non-interactive lisp execution: " +
                   std::get<std::string>(ret));
    }
    if(std::holds_alternative<op_ref>(ret)) {
      iff = generate_result(std::get<op_ref>(ret));
    } else {
      // The lisp expression didn't return anything. Check if a new expression
      // could follow and reset parsing there.
      std::cout << "Reset! Cur: " << cur_ << std::endl;
      if(cur_.type == token::LPar || cur_.type == token::Ident ||
         cur_.type == token::Exists || cur_.type == token::Forall)
        return parse_expr();
    }
  } else if(cur_.type == token::LPar || cur_.type == token::Ident ||
            cur_.type == token::Exists || cur_.type == token::Forall) {
    // The prior expression was parsed and works. Now comes a new expression.
    // This breaks the expression again! This means that the following
    // expression either is a lisp expression, or the whole thing is a lisp
    // expression. The latter is more reasonable, as it limits parsing
    // possibilities. This means we break out of this branch.
    return error(
      "No logical connective between expression and next expression!");
  }
  return iff;
}

result
boole::operator()() {
  // Begin reading, populate _next and _cur.
  if(next())
    next();

  result res;
  res = parse_expr();

  if(cur_.type != token::None) {
    std::string msg;
    if(!res) {
      msg = " - and error during parsing: " + res.message;
    }

    return error(
      std::string(
        "Did not finish parsing, last token was not consumed! Token: ") +
      token_type_str[cur_.type] + " (ident=" + cur_.ident + ")" + msg);
  }

  if(!res) {
    return res;
  }

  return res;
}

std::ostream&
operator<<(std::ostream& o, const boole::token& t) {
  o << token_type_str[t.type];
  if(t.type == boole::token::Ident)
    o << "{" << t.ident << "}";
  return o;
}
}
