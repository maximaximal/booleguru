#pragma once

#include <functional>
#include <optional>
#include <string_view>
#include <variant>

#include "base.hpp"

#include <booleguru/parse/type.hpp>

namespace booleguru::parse {
class cli : public base {
  virtual void init() override;

  public:
  using base::base;

  virtual ~cli();

  virtual result operator()() override;

  using parse_file_function
    = std::function<expression::op_ref(std::string_view)>;
  void parse_file_using(parse_file_function fun) {
    parse_file_function_ = fun;
  };

  parse::type output_type() const { return output_type_; };

  protected:
  struct internal;
  struct internal_deleter {
    void operator()(internal*);
  };
  std::unique_ptr<internal, internal_deleter> internal_;
  parse_file_function parse_file_function_;
  parse::type output_type_;
};
}
