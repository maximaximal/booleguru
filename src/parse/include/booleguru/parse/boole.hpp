#pragma once

#include <variant>

#include "base.hpp"
#include "sexpr-tracker.hpp"

namespace booleguru::parse {
class boole : public base {
  protected:
  struct token {
    enum token_type {
      Ident,
      SubstitutingLPar,
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
      Unknown,
      None
    };
    static const char* token_type_str[];
    static const char* token_type_sym[];

    constexpr inline bool is_binop_operator() const {
      switch(type) {
        case Equivalence:
        case Implies:
        case Seilpmi:
        case Or:
        case And:
        case Not:
          return true;
        default:
          return false;
      }
    }

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
  bool colon_ = false;

  bool unicode_op_prefix1_ = false;
  bool unicode_op_prefix2_ = false;

  token cur_;
  token next_;
  char c_ = 0;
  bool c_processed_ = true;
  bool c_appended_ = true;
  bool scanner_in_error_state_ = false;

  std::variant<bool, result> next(bool lispmode = false);

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
  result parse_lisp(int paren_level = 1,
                    std::optional<uint32_t> last_op = std::nullopt);

  friend std::ostream& operator<<(std::ostream&, const token& t);

  public:
  using base::base;

  virtual result operator()();
};
}
