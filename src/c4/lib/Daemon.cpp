#include "c4/Daemon.h"

namespace c4 {

int Daemon::start(CmdInput &ci) {
  // create listening socket
  int listenfd = socket(AF_INET, SOCK_STREAM, 0);
  if (listenfd < 0) {
    std::cerr << "failed to create listening socket" << std::endl;
    return 1;
  }

  // TODO:
  return 0;
}

int Daemon::shutdown() {
  // TODO:
  return 0;
}

} // namespace
