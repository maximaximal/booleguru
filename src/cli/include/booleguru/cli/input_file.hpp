#pragma once

#include <booleguru/util/type.hpp>

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace booleguru::expression {
class op_ref;
class op_manager;
}

namespace booleguru::parse {
class base;
}

namespace booleguru::lua {
class lua_context;
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

  struct internal;
  std::unique_ptr<internal> internal_;
  std::unique_ptr<parse::base> parser_;
  std::shared_ptr<expression::op_manager> ops_;
  std::shared_ptr<lua::lua_context> lua_;
  util::type type_ = util::type::boole;
  bool eval_ = false;

  std::istream& produce_istream();
  std::istream& produce_istream_from_popen(std::string command,
                                           std::string args);
  std::string find_in_path(std::string command);

  std::unique_ptr<parse::base> produce_parser(std::istream& is);

  bool file_matches_signature(std::string path, int* sig);

  public:
  input_file(std::string_view path,
             std::shared_ptr<expression::op_manager> ops,
             std::shared_ptr<lua::lua_context> lua);
  ~input_file();

  void type(util::type t) { type_ = t; }

  expression::op_ref process();
};
}
