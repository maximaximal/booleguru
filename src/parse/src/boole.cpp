#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/result.hpp>

#include <booleguru/expression/script_manager.hpp>
#include <booleguru/expression/var_manager.hpp>

#include <booleguru/lua/lua-context.hpp>

#include <exception>
#include <iostream>
#include <sstream>

#include <boole_lexer.h>
#include <boole_parser.h>

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {
struct parser_exception : public std::invalid_argument {
  size_t line;
  size_t col;
  parser_exception(const std::string& err, size_t line, size_t col)
    : std::invalid_argument(err)
    , line(line)
    , col(col){};
};

struct boole::internal {
  ANTLRInputStream input;
  boole_lexer lexer;
  CommonTokenStream tokens;
  boole_parser parser;

  struct error_listener : public ANTLRErrorListener {
    internal& i;

    error_listener(internal& i)
      : i(i) {}

    virtual void syntaxError(Recognizer* recognizer,
                             Token* offendingSymbol,
                             size_t line,
                             size_t charPositionInLine,
                             const std::string& msg,
                             std::exception_ptr e) override {
      (void)recognizer;
      (void)offendingSymbol;
      (void)e;
      throw parser_exception(msg, line, charPositionInLine);
    }
    virtual void reportAmbiguity(Parser* recognizer,
                                 const dfa::DFA& dfa,
                                 size_t startIndex,
                                 size_t stopIndex,
                                 bool exact,
                                 const antlrcpp::BitSet& ambigAlts,
                                 atn::ATNConfigSet* configs) override {
      (void)recognizer;
      (void)dfa;
      (void)startIndex;
      (void)stopIndex;
      (void)exact;
      (void)ambigAlts;
      (void)configs;
      throw parser_exception("Ambiguity!", 0, 0);
    }

    virtual void reportAttemptingFullContext(
      Parser* recognizer,
      const dfa::DFA& dfa,
      size_t startIndex,
      size_t stopIndex,
      const antlrcpp::BitSet& conflictingAlts,
      atn::ATNConfigSet* configs) override {
      (void)recognizer;
      (void)dfa;
      (void)startIndex;
      (void)stopIndex;
      (void)configs;
      (void)conflictingAlts;
      throw parser_exception("Ambiguity Full Context!", 0, 0);
    }

    virtual void reportContextSensitivity(Parser* recognizer,
                                          const dfa::DFA& dfa,
                                          size_t startIndex,
                                          size_t stopIndex,
                                          size_t prediction,
                                          atn::ATNConfigSet* configs) override {
      (void)recognizer;
      (void)dfa;
      (void)startIndex;
      (void)stopIndex;
      (void)prediction;
      (void)configs;
      throw parser_exception("Context Sensitivity!", 0, 0);
    }
  };

  error_listener listener;

  internal(std::istream& in)
    : input(in)
    , lexer(&input)
    , tokens(&lexer)
    , parser(&tokens)
    , listener(*this) {
    // Listen for errors and throw exceptions.
    lexer.removeErrorListeners();
    lexer.addErrorListener(&listener);
    parser.removeErrorListeners();
    parser.addErrorListener(&listener);

    // Ops are built directly in the parser using rule returns.
    parser.setBuildParseTree(false);
  }
};
void
boole::internal_deleter::operator()(internal* i) {
  delete i;
};

boole::~boole() = default;
void
boole::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
  internal_->parser.lua = lua_;
}
void
boole::eval(bool enable) {
  if(!internal_) {
    init();
  }
  internal_->parser.eval = enable;
}
result
boole::operator()() {
  if(!internal_) {
    init();
  }
  try {
    auto result = internal_->parser.formula();
    return generate_result(result->o);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
}
}
