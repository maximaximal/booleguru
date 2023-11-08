#pragma once

#include <functional>
#include <optional>
#include <string_view>
#include <variant>

#include "base.hpp"

#include <booleguru/util/type.hpp>

#include <booleguru/expression/expression_graph.hpp>

namespace booleguru::parse {
class cli : public base {
  virtual void init() override;

  public:
  using base::base;

  virtual ~cli();

  virtual result operator()() override;

  size_t fuzz_files();

  using parse_file_function
    = std::function<expression::op_ref(std::string_view, util::type type)>;
  void parse_file_using(parse_file_function fun) {
    parse_file_function_ = fun;
  };

  util::type output_type() const { return output_type_; };

  protected:
  struct internal;
  struct internal_deleter {
    void operator()(internal*);
  };
  std::unique_ptr<internal, internal_deleter> internal_;
  parse_file_function parse_file_function_;
  util::type output_type_;
};
}
