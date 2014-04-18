#include "c4/Server.h"

namespace c4 {

int Server::start(unsigned int port) {
  if (createListeningSock(port) < 0) {
    return -1;
  }

  // epoll
  int epollfd = epoll_create1(0);
  if (epollfd == -1) {
    errMsg = "can't initialize epoll";
    return -1;
  }

  // monitor listening socket
  struct epoll_event ev;
  ev.events = EPOLLIN;
  ev.data.fd = listenfd;
  if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listenfd, &ev) < 0) {
    errMsg = "failed to monitor listening socket.";
    return -1;
  }

  const int MAX_EVENTS = 1024;
  struct epoll_event events[MAX_EVENTS];
  struct sockaddr_in cliaddr;

  while (true) {
    int nevs = epoll_wait(epollfd, events, MAX_EVENTS, -1);
    for (int i = 0; i < nevs; i++) {
      // listening socket - accept new connections
      if (events[i].data.fd == listenfd) {
        socklen_t clilen = sizeof(cliaddr);
        int connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &clilen);
        if (connfd < 0) {
          std::cerr << "Failed to accept connection." << std::endl;
          continue;
        }

        createRequestBuffer(connfd);

        // monitor new connection
        ev.events = EPOLLIN;
        ev.data.fd = connfd;
        if (epoll_ctl(epollfd, EPOLL_CTL_ADD, connfd, &ev) < 0) {
          std::cerr << "Failed to monitor new connection fd#"
            << connfd << std::endl;
        }
        continue;
      }

      // handle socket communication
      // read data
      //if (evlist[i].flags & EVFILT_READ) {
        //int cbytes = read(evlist[i].ident, readBuffer, READ_BUFFER_MAX);
        //feed(evlist[i].ident, readBuffer, cbytes);

        //if (cbytes < 0) {
          // TODO:
          // read error
        //} else {
          // TODO:
          // send data to RequestBuffer
        //}
      //}
    }
  }

  return 0;
}

int Server::shutdown() {
  // TODO:
  return 0;
}

} // namespace
