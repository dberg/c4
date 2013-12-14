#include "c4/Daemon.h"
#include <strings.h>        // bzero
#include <sys/epoll.h>

namespace c4 {

int Daemon::start(CmdInput &ci) {
  // create listening socket
  int listenfd = socket(AF_INET, SOCK_STREAM, 0);
  if (listenfd < 0) {
    errMsg = "can't create listening socket";
    return -1;
  }

  struct sockaddr_in servaddr;
  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  servaddr.sin_port = htons(ci.getPort());

  // bind
  int resb = bind(listenfd, (struct sockaddr *) &servaddr, sizeof(servaddr));
  if (resb < 0) {
    errMsg = "can't bind listening socket to port: ";
    errMsg += itos(ci.getPort());
    return -1;
  }

  // listen
  const int LISTEN_BACKLOG = 1024;
  int resl = listen(listenfd, 1024);
  if (resl < 0) {
    errMsg = "can't listen on port ";
    errMsg += itos(ci.getPort());
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

int Daemon::shutdown() {
  // TODO:
  return 0;
}

} // namespace
