#include <booleguru/parse/uvl.hpp>
#include <booleguru/parse/result.hpp>

#include <uvlLexer.h>
#include <uvlParser.h>

#include "parser-internal.hpp"

using namespace booleguru::parse::antlr;
using namespace antlr4;

namespace booleguru::parse {

struct uvl::internal : public parser_internal<uvlLexer, uvlParser> {
  using parser_internal<uvlLexer, uvlParser>::parser_internal;
};

void
uvl::internal_deleter::operator()(uvl::internal* i) {
  delete i;
}

void
uvl::init() {
  internal_.reset(new internal(in_));
  internal_->parser.ops = ops_;
}

uvl::~uvl() = default;

result
uvl::operator()() {
  if(internal_ == nullptr)
    init();
  // Invoke Antlr runtime
  try {
    auto result = internal_->parser.featureModel();
    return generate_result(result->op);
  } catch(parser_exception& e) {
    line_ = e.line;
    column_ = e.col;
    return error(e.what());
  }
  return {};
}
}
