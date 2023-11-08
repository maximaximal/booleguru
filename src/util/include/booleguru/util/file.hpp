#pragma once

#include <string>

namespace booleguru::util {
bool
file_exists(const char* path);

bool
file_readable(const char* path);

bool
find_executable(const char* name, std::string& overwrite);

struct process {
  int infd[2];
  int outfd[2];
  FILE* infd_handle;
  FILE* outfd_handle;
  pid_t pid;
};
}
