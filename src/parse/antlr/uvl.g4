// This grammar is based on the big UVLParser Package on GitHub:
// https://github.com/Universal-Variability-Language/uvl-parser

/*
 * License for the INDENT DEDENT code: The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : python3-parser; an ANTLR4 grammar for Python 3
 *                https://github.com/bkiers/python3-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 */

grammar uvl;

tokens { INDENT, DEDENT }

@parser::header {
  #include <booleguru/expression/id.hpp>
  #include <booleguru/expression/op_manager.hpp>
  #include <booleguru/expression/var_manager.hpp>
  #include <booleguru/util/reverse.hpp>
  #include <booleguru/parse/uvl_helpers.hpp>

  using namespace booleguru;

  using op          = expression::op;
  using op_ref      = expression::op_ref;
  using op_id       = expression::op_id;
  using op_type     = expression::op_type;
  using op_manager  = expression::op_manager;
  using var_ref     = expression::var_ref;
  using var_id      = expression::var_id;
  using var_manager = expression::var_manager;
}

@lexer::header {
  #include "uvlParser.h"
}

@lexer::members {
  std::vector<std::unique_ptr<antlr4::Token>> m_tokens;
  std::stack<int> m_indents;
  int m_opened = 0;
  std::unique_ptr<antlr4::Token> m_pLastToken = nullptr;

  virtual void emit(std::unique_ptr<antlr4::Token> newToken) override {
    m_tokens.push_back(cloneToken(newToken));
    setToken(std::move(newToken));
  }

  std::unique_ptr<antlr4::Token> nextToken() override {
    // Check if the end-of-file is ahead and there are still some DEDENTS expected.
    if (_input->LA(1) == EOF && !m_indents.empty()) {
      // Remove any trailing EOF tokens from our buffer.
      for (int i = m_tokens.size() - 1; i >= 0; i--) {
        if (m_tokens[i]->getType() == EOF) {
          m_tokens.erase(m_tokens.begin() + i);
        }
      }

      // First emit an extra line break that serves as the end of the statement.
      emit(commonToken(uvlLexer::NEWLINE, "\n"));

      // Now emit as much DEDENT tokens as needed.
      while (!m_indents.empty()) {
        emit(createDedent());
        m_indents.pop();
      }

      // Put the EOF back on the token stream.
      emit(commonToken(EOF, "<EOF>"));
    }

    std::unique_ptr<antlr4::Token> next = Lexer::nextToken();

    if (next->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
      // Keep track of the last token on the default channel.
      m_pLastToken = cloneToken(next);
    }

    if (!m_tokens.empty())
    {
      next = std::move(*m_tokens.begin());
      m_tokens.erase(m_tokens.begin());
    }

    return next;
  }

  private:
  std::unique_ptr<antlr4::Token> createDedent() {
    std::unique_ptr<antlr4::CommonToken> dedent = commonToken(uvlParser::DEDENT, "");
    return dedent;
  }

  std::unique_ptr<antlr4::CommonToken> commonToken(size_t type, const std::string& text) {
    int stop = getCharIndex() - 1;
    int start = text.empty() ? stop : stop - text.size() + 1;
    return _factory->create({ this, _input }, type, text, DEFAULT_TOKEN_CHANNEL, start, stop, m_pLastToken->getLine(), m_pLastToken->getCharPositionInLine());
  }

  std::unique_ptr<antlr4::CommonToken> cloneToken(const std::unique_ptr<antlr4::Token>& source) {
      return _factory->create({ this, _input }, source->getType(), source->getText(), source->getChannel(), source->getStartIndex(), source->getStopIndex(), source->getLine(), source->getCharPositionInLine());
  }


  // Calculates the indentation of the provided spaces, taking the
  // following rules into account:
  //
  // "Tabs are replaced (from left to right) by one to eight spaces
  //  such that the total number of characters up to and including
  //  the replacement is a multiple of eight [...]"
  //
  //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
  static int getIndentationCount(const std::string& spaces) {
    int count = 0;
    for (char ch : spaces) {
      switch (ch) {
        case '\t':
          count += 8 - (count % 8);
          break;
        default:
          // A normal space char.
          count++;
      }
    }

    return count;
  }

  bool atStartOfInput() {
    return getCharPositionInLine() == 0 && getLine() == 1;
  }
}

@parser::members {
  std::shared_ptr<op_manager> ops;
  op_id conj = 0;
}


featureModel returns [op_ref op]
  : namespace? NEWLINE? includes? NEWLINE? imports? NEWLINE?
        { conj = 0; }
    ( f=features { $op = (*ops)[$f.o]; }) NEWLINE?
    ( c=constraints { $op = $op && $c.op; })? EOF;

includes: 'include' NEWLINE INDENT includeLine* DEDENT;
includeLine: languageLevel NEWLINE;

namespace: 'namespace' reference;

imports: 'imports' NEWLINE INDENT importLine* DEDENT;
importLine: ns=reference ('as' alias=reference)? NEWLINE;

features returns [op_id o]
  : 'features' NEWLINE INDENT f=feature { $o = ops->encode_conjunct($f.o, conj); } DEDENT;

feature returns [op_id o]
  : featureType? r=reference { op_id var = $r.o; $o = $r.o; }
    featureCardinality? attributes? NEWLINE
    (INDENT ( g=group[var] { conj = ops->encode_conjunct(conj, $g.o); } )+ DEDENT)?;

group [ op_id f ] returns [ op_id o ]
    : ORGROUP g=groupSpec[std::make_shared<uvl_subfeature_or>(ops, f)] { $o = $g.o; }          # OrGroup
    | ALTERNATIVE g=groupSpec[std::make_shared<uvl_subfeature_xor>(ops, f)] { $o = $g.o; } # AlternativeGroup
    | OPTIONAL g=groupSpec[std::make_shared<uvl_subfeature_optional>(ops, f)] { $o = $g.o; }    # OptionalGroup
    | MANDATORY g=groupSpec[std::make_shared<uvl_subfeature_mandatory>(ops, f)] { $o = $g.o; }   # MandatoryGroup
    | CARDINALITY { notifyErrorListeners("cardinality not supported"); } groupSpec[std::make_shared<uvl_subfeature_or>(ops, f)]    # CardinalityGroup
    ;

groupSpec [ std::shared_ptr<uvl_subfeature> s ] returns [ op_id o ]
  : NEWLINE INDENT (f=feature {$s->add($f.o);})+ {$o = $s->finalize();} DEDENT;

featureCardinality: 'cardinality' CARDINALITY;

attributes: OPEN_BRACE (attribute (COMMA attribute)*)? CLOSE_BRACE;

attribute
    : valueAttribute
    | constraintAttribute;

valueAttribute: key value?;

key: id;
value: BOOLEAN | FLOAT | INTEGER | STRING | attributes | vector;
vector: OPEN_BRACK (value (COMMA value)*)? CLOSE_BRACK;

constraintAttribute
    : 'constraint' constraint               # SingleConstraintAttribute
    | 'constraints' constraintList          # ListConstraintAttribute
    ;
constraintList: OPEN_BRACK (constraint (COMMA constraint)*)? CLOSE_BRACK;

constraints returns [ op_ref op ]
  : 'constraints' NEWLINE INDENT { $op = op_ref(); }
    ( c=constraintLine { $op = $op && (*ops)[$c.o]; } )* DEDENT;

constraintLine returns [ op_id o ]: c=constraint { $o = $c.o; } NEWLINE;

constraint returns [ op_id o ]
    : r=reference { $o = $r.o; }                             # LiteralConstraint
    | OPEN_PAREN c=constraint { $o = $c.o; } CLOSE_PAREN     # ParenthesisConstraint
    | NOT c=constraint  { $o = ops->encode_not($c.o); }                      # NotConstraint
    | l=constraint AND r=constraint { $o = ops->encode_and($l.o, $r.o); }             # AndConstraint
    | l=constraint OR r=constraint { $o = ops->encode_or($l.o, $r.o); }             # OrConstraint
    | l=constraint IMPLICATION r=constraint { $o = ops->encode_impl($l.o, $r.o); }    # ImplicationConstraint
    | l=constraint EQUIVALENCE r=constraint { $o = ops->encode_equi($l.o, $r.o); }    # EquivalenceConstraint
	;

equation
    : expression EQUAL expression           # EqualEquation
    | expression LOWER expression           # LowerEquation
    | expression GREATER expression         # GreaterEquation
    | expression LOWER_EQUALS expression    # LowerEqualsEquation
    | expression GREATER_EQUALS expression  # GreaterEqualsEquation
    | expression NOT_EQUALS expression      # NotEqualsEquation
    ;

expression:
    FLOAT                                   # FloatLiteralExpression
    | INTEGER                               # IntegerLiteralExpression
    | STRING                                # StringLiteralExpression
    | aggregateFunction                     # AggregateFunctionExpression
    | reference                             # LiteralExpression
    | OPEN_PAREN expression CLOSE_PAREN     # BracketExpression
    | expression ADD expression             # AddExpression
    | expression SUB expression             # SubExpression
    | expression MUL expression             # MulExpression
    | expression DIV expression             # DivExpression
    ;

aggregateFunction
    : 'sum' OPEN_PAREN (reference COMMA)? reference CLOSE_PAREN    # SumAggregateFunction
    | 'avg' OPEN_PAREN (reference COMMA)? reference CLOSE_PAREN    # AvgAggregateFunction
    | stringAggregateFunction                                      # StringAggregateFunctionExpression
    | numericAggregateFunction                                     # NumericAggregateFunctionExpression
    ;

stringAggregateFunction
    : 'len' OPEN_PAREN reference CLOSE_PAREN        # LengthAggregateFunction
    ;

numericAggregateFunction
    : 'floor' OPEN_PAREN reference CLOSE_PAREN      # FloorAggregateFunction
    | 'ceil' OPEN_PAREN reference CLOSE_PAREN       # CeilAggregateFunction
    ;

reference returns [op_id o]: n=id { $o = ops->get_id(op(op_type::Var, ops->vars().get_id({ $n.text }), 0, 0)); };
id: ID_STRICT | ID_NOT_STRICT;
featureType
  : 'String' { notifyErrorListeners("only booleans supported!"); }
  | 'Integer' { notifyErrorListeners("only booleans supported!"); }
  | BOOLEAN_KEY
  | 'Real' { notifyErrorListeners("only booleans supported!"); };



languageLevel: majorLevel ('.' (minorLevel | '*'))?;
majorLevel: BOOLEAN_KEY | 'Arithmetic' | 'Type';
minorLevel: 'group-cardinality' | 'feature-cardinality' | 'aggregate-function' | 'string-constraints';

ORGROUP: 'or';
ALTERNATIVE: 'alternative';
OPTIONAL: 'optional';
MANDATORY: 'mandatory';
CARDINALITY: OPEN_BRACK INTEGER ('..' (INTEGER | '*'))? CLOSE_BRACK;

NOT: '!';
AND: '&';
OR: '|';
EQUIVALENCE: '<=>';
IMPLICATION: '=>';

EQUAL: '==';
LOWER: '<';
LOWER_EQUALS: '<=';
GREATER: '>';
GREATER_EQUALS: '>=';
NOT_EQUALS: '!=';

DIV: '/';
MUL: '*';
ADD: '+';
SUB: '-';

FLOAT: '-'?[0-9]*[.][0-9]+;
INTEGER: '0' | '-'?[1-9][0-9]*;
BOOLEAN: 'true' | 'false';

BOOLEAN_KEY : 'Boolean';

COMMA: ',';

ID_NOT_STRICT: '"'~[\r\n".]+'"';
ID_STRICT: [a-zA-Z]([a-zA-Z0-9_] | '#' | '§' | '%' | '?' | '\\' | '\'' | 'ä' | 'ü' | 'ö' | 'ß' | ';')*;

STRING: '\''~[\r\n'.]+'\'';

SKIP_
  : ( SPACES | COMMENT ) -> skip
  ;

 fragment COMMENT
  : '//' ~[\r\n\f]*
  | OPEN_COMMENT .* CLOSE_COMMENT
  ;

  fragment SPACES
   : [ \t]+
   ;

OPEN_PAREN : '(' ;
CLOSE_PAREN : ')' ;
OPEN_BRACK : '[' ;
CLOSE_BRACK : ']' ;
OPEN_BRACE : '{' ;
CLOSE_BRACE : '}' ;
OPEN_COMMENT: '/*' ;
CLOSE_COMMENT: '*/' ;

NEWLINE
 : ( {atStartOfInput()}?   SPACES
   | ( '\r'? '\n' | '\r' | '\f' ) SPACES?
   )
   {
     {	 
     std::string newLine, spaces;
     std::string text = getText();
     for(char c : text)
     {
       if ((c == '\r') || (c == '\n') || (c == '\f'))
         newLine.push_back(c);
       else
         spaces.push_back(c);
     }


     // Strip newlines inside open clauses except if we are near EOF. We keep NEWLINEs near EOF to
     // satisfy the final newline needed by the single_put rule used by the REPL.
     int next = _input->LA(1);
     int nextnext = _input->LA(2);
     if (m_opened > 0 || (nextnext != -1 && (next == '\r' || next == '\n' || next == '\f' || next == '#'))) {
       // If we're inside a list or on a blank line, ignore all indents, 
       // dedents and line breaks.
       skip();
     }
     else {
       emit(commonToken(NEWLINE, newLine));
       int indent = getIndentationCount(spaces);
       int previous = m_indents.empty() ? 0 : m_indents.top();
       if (indent == previous) {
         // skip indents of the same size as the present indent-size
         skip();
       }
       else if (indent > previous) {
         m_indents.push(indent);
         emit(commonToken(uvlParser::INDENT, spaces));
       }
       else {
         // Possibly emit more than 1 DEDENT token.
         while(!m_indents.empty() && m_indents.top() > indent) {
           emit(createDedent());
           m_indents.pop();
         }
       }
     }
     }
   }
 ;
