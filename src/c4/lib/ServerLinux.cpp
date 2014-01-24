#include "c4/Server.h"

namespace c4 {

int Server::start(CmdInput &ci) {
  if (createListeningSock(ci) < 0) {
    return -1;
  }

  // epoll
  int epollfd = epoll_create1(0);
  if (epollfd == -1) {
    errMsg = "can't initialize epoll";
    return -1;
  }

  // TODO:
  return 0;
}

int Server::shutdown() {
  // TODO:
  return 0;
}

} // namespace
