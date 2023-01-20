#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <booleguru/cli/input_file.hpp>
#include <booleguru/expression/op_manager.hpp>
#include <booleguru/expression/var_manager.hpp>
#include <booleguru/parse/base.hpp>
#include <booleguru/parse/boole.hpp>
#include <booleguru/parse/qdimacs.hpp>
#include <booleguru/parse/result.hpp>
#include <booleguru/parse/smtlib2.hpp>

namespace booleguru::cli {
static int xzsig[] = { 0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00, 0x00, EOF };
static int bz2sig[] = { 0x42, 0x5A, 0x68, EOF };
static int gzsig[] = { 0x1F, 0x8B, EOF };
static int sig7z[] = { 0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C, EOF };
static int lzmasig[] = { 0x5D, 0x00, 0x00, 0x80, 0x00, EOF };
static int zstdsig[] = { 0xFD, 0x2F, 0xB5, 0x28, EOF };

// Taken from https://stackoverflow.com/a/12342830
class stdiobuf : public std::streambuf {
  private:
  FILE* d_file;
  char d_buffer[8192];

  public:
  stdiobuf(FILE* file)
    : d_file(file) {}
  int underflow() {
    if(this->gptr() == this->egptr() && this->d_file) {
      size_t size = fread(this->d_buffer, 1, 8192, this->d_file);
      this->setg(this->d_buffer, this->d_buffer, this->d_buffer + size);
    }
    return this->gptr() == this->egptr()
             ? traits_type::eof()
             : traits_type::to_int_type(*this->gptr());
  }
};

struct input_file::internal {
  struct popen_variant {
    FILE* popen_handle = nullptr;
    stdiobuf popen_stdio_filebuf;
    std::istream istream;
    bool use_pclose;

    popen_variant(FILE* popen_handle, bool use_pclose = true)
      : popen_handle(popen_handle)
      , popen_stdio_filebuf(stdiobuf(popen_handle))
      , istream(&popen_stdio_filebuf)
      , use_pclose(use_pclose) {}
    ~popen_variant() {
      if(popen_handle) {
        if(use_pclose)
          pclose(popen_handle);
        else
          fclose(popen_handle);
      }
    }
  };

  std::variant<popen_variant, std::ifstream> variants;

  std::istream& popen_istream() {
    return std::get<popen_variant>(variants).istream;
  }
  std::ifstream& ifstream() { return std::get<std::ifstream>(variants); }

  int fd() { return fileno(std::get<popen_variant>(variants).popen_handle); }

  internal(FILE* popen_handle, bool pclose = true)
    : variants(std::in_place_type<popen_variant>, popen_handle) {}
  internal(std::string path)
    : internal(fopen(path.c_str(), "r"), false) {
  }
};

input_file::input_file(std::string_view path,
                       const std::vector<argument>& args,
                       std::shared_ptr<expression::op_manager> ops)
  : path_(path)
  , ops_(ops) {
  for(const auto arg : args) {
    args_[arg.keyword] = arg.param;

    if(arg.keyword == argument::type)
      check_filename_extension_ = false;
  }
}

input_file::~input_file() {}

bool
input_file::file_matches_signature(std::string path, int* sig) {
  std::ifstream in(path, std::ios::binary);
  for(int* c = sig; *c != EOF; ++c) {
    unsigned char r;
    in >> r;
    if(*c != r)
      return false;
  }
  return true;
}

expression::op_ref
input_file::process() {
  // Have to check whether to extract the given file. Then, to check what parser
  // to use. Afterwards actually parse the file and return the resulting
  // expression.

  parser_ = produce_parser(produce_istream());
  std::string_view variable_namespace =
    std::get<std::string_view>(args_[argument::variable_namespace]);
  if(variable_namespace.length() > 0) {
    ops_->vars().push_namespace(std::string(variable_namespace));
  }
  auto res = (*parser_)();
  if(variable_namespace.length() > 0) {
    ops_->vars().pop_namespace();
  }
  if(!res) {
    std::stringstream s;
    s << res;
    throw parse_error("Parse error! Result: " + s.str() +
                      ", message: " + res.message);
  }
  return *res;
}

std::istream&
input_file::produce_istream() {
  if(path_ == "-" || path_ == "/dev/stdin") {
    name_ = "-";
    check_filename_extension_ = false;
    return std::cin;
  }

  if(!std::filesystem::exists(path_)) {
    throw input_file_not_found(path_);
  }

  if(path_.ends_with(".xz") && file_matches_signature(path_, xzsig)) {
    name_ = path_.substr(0, path_.length() - 3);
    return produce_istream_from_popen("xz", "-c -d");
  } else if(path_.ends_with(".lzma") &&
            file_matches_signature(path_, lzmasig)) {
    name_ = path_.substr(0, path_.length() - 5);
    return produce_istream_from_popen("lzma", "-c -d");
  } else if(path_.ends_with(".bz2") && file_matches_signature(path_, bz2sig)) {
    name_ = path_.substr(0, path_.length() - 4);
    return produce_istream_from_popen("bzip2", "-c -d");
  } else if(path_.ends_with(".gz") && file_matches_signature(path_, gzsig)) {
    name_ = path_.substr(0, path_.length() - 3);
    return produce_istream_from_popen("gzip", "-c -d");
  } else if(path_.ends_with(".zst") && file_matches_signature(path_, zstdsig)) {
    name_ = path_.substr(0, path_.length() - 4);
    return produce_istream_from_popen("zstd", "-c -d");
  } else if(path_.ends_with(".7z") && file_matches_signature(path_, sig7z)) {
    name_ = path_.substr(0, path_.length() - 3);
    return produce_istream_from_popen("7z", "x -so");
  }
  name_ = path_;
  internal_ = std::make_unique<internal>(path_);
  return internal_->popen_istream();
}

std::istream&
input_file::produce_istream_from_popen(std::string command, std::string args) {
  using namespace std::string_literals;

  std::string abs_command = find_in_path(command);
  std::string cmd = abs_command + " " + args + " " + path_ + " 2>/dev/null";
  FILE* handle = popen(cmd.c_str(), "r");
  if(!handle) {
    throw std::runtime_error("Coult not popen()! Error: "s + strerror(errno));
  }
  internal_ = std::make_unique<internal>(handle);
  return internal_->popen_istream();
}

std::unique_ptr<parse::base>
input_file::produce_parser(std::istream& is) {
  if(check_filename_extension_) {
    if(name_.ends_with(".smtlib2") || name_.ends_with(".smt") ||
       name_.ends_with(".smtlib")) {
      args_[argument::type] = argument::smtlib2;
    } else if(name_.ends_with(".boole") || name_.ends_with(".limboole")) {
      args_[argument::type] = argument::boole;
    } else if(name_.ends_with(".dimacs") || name_.ends_with(".qdimacs")) {
      args_[argument::type] = argument::qdimacs;
    } else if(name_.ends_with(".qcir")) {
      args_[argument::type] = argument::qcir;
    }
  }

  switch(std::get<argument::input_types>(args_[argument::type])) {
    case argument::smtlib2:
      return std::make_unique<parse::smtlib2>(internal_->fd(), ops_);
    case argument::qcir:
      throw std::runtime_error("QCIR Not supported yet!");
    case argument::boole: {
      is >> std::noskipws;
      auto boole = std::make_unique<parse::boole>(is, ops_);
      boole->eval(std::get<bool>(args_[argument::eval]));
      return boole;
    }
    case argument::qdimacs:
      is >> std::noskipws;
      return std::make_unique<parse::qdimacs>(is, ops_);
  }
}

std::string
input_file::find_in_path(std::string command) {
  using namespace std::string_literals;

  std::string_view path = std::getenv("PATH");

  size_t pos = 0;
  std::string result;
  while((pos = path.find(":")) != std::string::npos) {
    result = path.substr(0, pos);
    result += "/" + command;
    if(std::filesystem::exists(result)) {
      return result;
    }
    path = path.substr(pos + 1);
  }

  throw std::runtime_error("Could not find "s + command + " in PATH!");
}

}
