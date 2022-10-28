#pragma once

#include "base.hpp"

namespace booleguru::parse {
class boole : public base {
  protected:
  struct token {
    enum token_type {
      Script,
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

  int script_ = 0;
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

  bool next();

  result parse_expr();

  public:
  using base::base;

  virtual result operator()();
};
}
