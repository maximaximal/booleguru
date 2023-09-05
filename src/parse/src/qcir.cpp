// Author: Marcel Simader (marcel.simader@jku.at)
// Date: 26.07.2023
// (c) Marcel Simader 2023, Johannes Kepler Universität Linz

#include <booleguru/parse/qcir.hpp>
#include <booleguru/parse/result.hpp>

#include <qcir_lexer.h>
#include <qcir_parser.h>

#include "parser-internal.hpp"

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {

struct qcir::internal : public parser_internal<qcir_lexer, qcir_parser> {
  using parser_internal<qcir_lexer, qcir_parser>::parser_internal;
};

void
qcir::internal_deleter::operator()(qcir::internal* i) {
  delete i;
}

void
qcir::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
}

qcir::~qcir() = default;

result
qcir::operator()() {
  if(internal_ == nullptr)
    init();
  // Invoke Antlr runtime
  try {
    auto result = internal_->parser.formula();
    return generate_result(result->op);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
  return {};
}
}
