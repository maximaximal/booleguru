#pragma once

#include <map>
#include <memory>
#include <vector>

#include "argument.hpp"

namespace booleguru::expression {
class op_ref;
class op_manager;
}

namespace booleguru::parse {
class base;
}

namespace booleguru::cli {
struct input_file_not_found : public std::runtime_error {
  using std::runtime_error::runtime_error;
};

struct parse_error : public std::runtime_error {
  using std::runtime_error::runtime_error;
};

class input_file {
  std::string path_;
  std::string name_;

  // Default arguments for input files.
  argument::param_variant args_[argument::keywords::count_] = {
    argument::boole, /* type */
    false            /* eval */
  };

  struct internal;
  std::unique_ptr<internal> internal_;
  std::unique_ptr<parse::base> parser_;
  std::shared_ptr<expression::op_manager> ops_;

  std::istream& produce_istream();
  std::istream& produce_istream_from_popen(std::string command,
                                           std::string args);
  std::string find_in_path(std::string command);

  std::unique_ptr<parse::base> produce_parser(std::istream& is);
  bool check_filename_extension_ = true;

  bool file_matches_signature(std::string path, int* sig);

  public:
  input_file(std::string_view path,
             const std::vector<argument>& args,
             std::shared_ptr<expression::op_manager> ops);
  ~input_file();

  expression::op_ref process();
};
}
