#pragma once

#include <stdexcept>
#include <string_view>
#include <variant>

namespace booleguru::cli {
struct unknown_argument : public std::invalid_argument {
  using std::invalid_argument::invalid_argument;
};

struct argument {
  argument(std::string_view arg, std::string_view param);
  argument(std::string_view arg);

  const std::string_view arg;

  enum keywords {
    type,
    eval,
    variable_namespace,
    count_,
  };

  enum input_types { qcir, smtlib2, boole, qdimacs, none };

  keywords keyword;

  using param_variant = std::variant<std::string_view, bool, int, input_types>;
  param_variant param;
};
}
