#include "djp/Daemon.h"
#include <iostream>

using namespace djp;

int main(int argc, const char **argv) {
  Daemon daemon(argc, argv);
  if (daemon.start()) {
    std::cerr << daemon.getError();
    return 1;
  }
  return 0;
}
