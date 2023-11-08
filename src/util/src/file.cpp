#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {
#include <unistd.h>

#ifdef WIN32
#include <io.h>
#define F_OK 0
#define access _access
#endif
}

#include <booleguru/util/file.hpp>

namespace booleguru::util {
// File existence and find_executable taken and adapted from MIT-licensed
// Kissat.
bool
file_exists(const char* path) {
  if(!path)
    return false;
  if(access(path, F_OK))
    return false;
  return true;
}

bool
file_readable(const char* path) {
  if(!path)
    return false;
  if(access(path, R_OK))
    return false;
  return true;
}

bool
find_executable(const char* name, std::string& overwrite) {
  const size_t name_len = strlen(name);
  const char* environment = getenv("PATH");
  if(!environment)
    return false;
  const size_t dirs_len = strlen(environment);
  char* dirs = (char*)malloc(dirs_len + 1);
  if(!dirs)
    return false;
  strcpy(dirs, environment);
  bool res = false;
  const char* end = dirs + dirs_len + 1;
  for(char *dir = dirs, *q; !res && dir != end; dir = q) {
    for(q = dir; *q && *q != ':'; q++)
      assert(q + 1 < end);
    *q++ = 0;
    const size_t path_len = (q - dir) + name_len;
    char* path = (char*)malloc(path_len + 1);
    if(!path) {
      free(dirs);
      return false;
    }
    snprintf(path, path_len + 1, "%s/%s", dir, name);
    assert(strlen(path) == path_len);
    res = file_readable(path);
    overwrite = std::string(path);
    free(path);
  }
  free(dirs);
  return res;
}
}
