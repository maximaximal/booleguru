#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

namespace booleguru::parse {
boole::token::token() {}
boole::token::~token() {}

const char* token_type_str[] = { "Script",  "Ident",  "LPar",        "RPar",
                                 "Exists",  "Forall", "Equivalence", "Implies",
                                 "Seilpmi", "Or",     "And",         "Not",
                                 "None" };
const char* token_type_sym[] = { "Script", "_",  "(", ")", "?", "@", "<->",
                                 "->",     "<-", "|", "&", "!", "." };

boole::token&
boole::token::operator=(token&& o) {
  if(o.type == Ident || o.type == Script) {
    ident = std::move(o.ident);
  } else
    ident.clear();

  type = o.type;
  line = o.line;
  col = o.col;

  return *this;
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

    if(c_ != '[' && c_ != ']' && script_ > 0) {
      c_processed_ = true;
      ident.push_back(c_);
      continue;
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

      if(script_ == 0 && ident_end_detect())
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
        case '[':
          if(script_ > 0)
            ident.push_back(c_);
          ++script_;
          continue;
        case ']':
          --script_;
          if(script_ == 0) {
            type = token::Script;
            return true;
          } else {
            if(script_ > 0)
              ident.push_back(c_);
            continue;
          }
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
          return error(std::string("Invalid character '") + c_ + "'!");
      }

      return true;
    }
  }
}

result
boole::parse_expr() {
  return error("Expr Unimplemented");
}

result
boole::operator()() {
  // Begin reading, populate _next and _cur.
  if(next())
    next();

  result res;
  res = parse_expr();

  if(cur_.type != token::None) {
    return error(
      std::string(
        "Did not finish parsing, last token was not consumed! Token: ") +
      token_type_str[cur_.type]);
  }

  return res;
}
}
