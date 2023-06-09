#pragma once

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

namespace booleguru::util {
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
}
