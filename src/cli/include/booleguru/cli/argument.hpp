#pragma once

#include <string_view>
#include <variant>

namespace booleguru::cli {
struct argument {
  argument(std::string_view arg, std::string_view param);
  argument(std::string_view arg);

  const std::string_view arg;

  enum keywords {
    type,
    eval,
    unknown,
  };

  enum input_types { qcir, smtlib2, boole, qdimacs };

  keywords keyword;

  using param_variant = std::variant<std::string_view, bool, int, input_types>;
  param_variant param;
};
}
