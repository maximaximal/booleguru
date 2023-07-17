#pragma once

#include <cstddef>
#include <cstdint>

#include <antlr4-runtime.h>

namespace booleguru::parse {
struct parser_exception : public std::invalid_argument {
  size_t line;
  size_t col;
  parser_exception(const std::string& err, size_t line, size_t col)
    : std::invalid_argument(err)
    , line(line)
    , col(col){};
};

template<class Lexer, class Parser>
struct parser_internal {
  using self = parser_internal<Lexer, Parser>;
  antlr4::ANTLRInputStream input;
  Lexer lexer;
  antlr4::CommonTokenStream tokens;
  Parser parser;

  struct error_listener : public ::antlr4::ANTLRErrorListener {
    self& i;

    error_listener(self& i)
      : i(i) {}

    virtual void syntaxError(antlr4::Recognizer* recognizer,
                             antlr4::Token* offendingSymbol,
                             size_t line,
                             size_t charPositionInLine,
                             const std::string& msg,
                             std::exception_ptr e) override {
      (void)recognizer;
      (void)offendingSymbol;
      (void)e;
      throw parser_exception(msg, line, charPositionInLine);
    }
    virtual void reportAmbiguity(antlr4::Parser* recognizer,
                                 const antlr4::dfa::DFA& dfa,
                                 size_t startIndex,
                                 size_t stopIndex,
                                 bool exact,
                                 const antlrcpp::BitSet& ambigAlts,
                                 antlr4::atn::ATNConfigSet* configs) override {
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
      antlr4::Parser* recognizer,
      const antlr4::dfa::DFA& dfa,
      size_t startIndex,
      size_t stopIndex,
      const antlrcpp::BitSet& conflictingAlts,
      antlr4::atn::ATNConfigSet* configs) override {
      (void)recognizer;
      (void)dfa;
      (void)startIndex;
      (void)stopIndex;
      (void)configs;
      (void)conflictingAlts;
      throw parser_exception("Ambiguity Full Context!", 0, 0);
    }

    virtual void reportContextSensitivity(
      antlr4::Parser* recognizer,
      const antlr4::dfa::DFA& dfa,
      size_t startIndex,
      size_t stopIndex,
      size_t prediction,
      antlr4::atn::ATNConfigSet* configs) override {
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

  parser_internal(std::istream& in)
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

    parser.template getInterpreter<antlr4::atn::ParserATNSimulator>()
      ->setPredictionMode(antlr4::atn::PredictionMode::SLL);
  }
};
}
