#ifndef __UTIL_H__
#define __UTIL_H__
#include <cstring>
#include <dlfcn.h>
#include <stdint.h>
#include <libgen.h>
#include <string>

namespace c4 {
std::string getCurrentDir() {
  Dl_info dlinfo;
  void *p = (void *) (intptr_t) getCurrentDir;
  if (dladdr(p, &dlinfo)) {
    char *filename = strdup(dlinfo.dli_fname);
    std::string dir = dirname(filename);
    free(filename);
    return dir;
  }

  // failed to get current directory
  return "";
}

std::string current_dir = getCurrentDir();

} // namespace
#endif
