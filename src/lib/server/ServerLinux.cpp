#include "c4/server/Server.h"

namespace c4 {

int Server::start(unsigned int port) {
  if (createListeningSock(port) < 0) {
    log(LOG_ERROR, "Failed to create listening socket");
    return -1;
  }

  // epoll
  int epollfd = epoll_create1(0);
  if (epollfd == -1) {
    log(LOG_ERROR, "Failed to initialize epoll.");
    return -1;
  }

  // monitor listening socket
  struct epoll_event ev;
  ev.events = EPOLLIN;
  ev.data.fd = listenfd;
  if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listenfd, &ev) < 0) {
    log(LOG_ERROR, "Failed to monitor listening socket");
    return -1;
  }

  const int MAX_EVENTS = 1024;
  struct epoll_event events[MAX_EVENTS];

  struct sockaddr_in cliaddr;
  const int READ_BUFFER_MAX = 1024;
  char readBuffer[READ_BUFFER_MAX];

  while (true) {
    int nevs = epoll_wait(epollfd, events, MAX_EVENTS, -1);
    for (int i = 0; i < nevs; i++) {
      // listening socket - accept new connections
      if (events[i].data.fd == listenfd) {
        socklen_t clilen = sizeof(cliaddr);
        int connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &clilen);
        if (connfd < 0) {
          log(LOG_ERROR, "Failed to accept connection");
          continue;
        }

        log(LOG_INFO, "New connection fd#" + itos(connfd));

        createRequestBuffer(connfd);

        // monitor new connection
        ev.events = EPOLLIN;
        ev.data.fd = connfd;
        if (epoll_ctl(epollfd, EPOLL_CTL_ADD, connfd, &ev) < 0) {
          log(LOG_ERROR, "Failed to monitor new connection fd#" + itos(connfd));
        }
        continue;
      }

      // handle socket communication
      // read data
      if (events[i].events & EPOLLIN) {
        int socket = events[i].data.fd;
        int cbytes = read(socket, readBuffer, READ_BUFFER_MAX);
        if (cbytes > 0) {
          if (feed(socket, readBuffer, cbytes)) {
              // we have a complete request
              spRequest request = getRequest(events[i].data.fd);
              spResponse response = spResponse(new Response);
              projHandler->process(request, response);
              responses[socket].push_back(response);
          }
        }

        // TODO:
        // connection closed by the client
        // stop monitoring this socket
        if (cbytes < 0 || events[i].events & EPOLLERR) {
          close(events[i].data.fd);
        }

        // TODO:
        // write data
      }
    }
  }

  return 0;
}

int Server::shutdown() {
  // TODO:
  return 0;
}

} // namespace
