#include <dlfcn.h>
#include <stdint.h>
#include <libgen.h>
#include <string>

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

#include "ParserBinTest.cpp"
#include "ParserBinScalaTest.cpp"

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
