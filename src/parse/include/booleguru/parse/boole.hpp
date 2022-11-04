#pragma once

#include "base.hpp"
#include "sexpr-tracker.hpp"

namespace booleguru::parse {
class boole : public base {
  protected:
  struct token {
    enum token_type {
      Ident,
      LPar,
      RPar,
      Exists,
      Forall,
      Equivalence,
      Implies,
      Seilpmi,
      Or,
      And,
      Not,
      None
    };
    static const char* token_type_str[];
    static const char* token_type_sym[];

    token_type type = None;

    token();
    ~token();
    token& operator=(token&& o);

    std::string ident;
    uint64_t line = 0, col = 0;
  };

  token::token_type token_type_from_op_type(expression::op_type t);

  sexpr_tracker sexp_;

  bool comment_ = false;

  bool implies_ = false;
  bool seilpmi_ = false;
  bool equivalent_ = false;
  bool middledash_ = false;

  bool unicode_op_prefix1_ = false;
  bool unicode_op_prefix2_ = false;

  token cur_;
  token next_;
  char c_ = 0;
  bool c_processed_ = true;
  bool c_appended_ = true;

  bool next();

  template<expression::op_type type, typename Functor>
  result parse_assoc_op(Functor next);

  result parse_iff();
  result parse_implies();
  result parse_seilpmi();
  result parse_or();
  result parse_and();
  result parse_not();
  result parse_basic();
  result parse_expr();

  friend std::ostream& operator<<(std::ostream&, const token& t);

  public:
  using base::base;

  virtual result operator()();
};
}
