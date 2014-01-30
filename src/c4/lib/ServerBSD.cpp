#include "c4/Server.h"

namespace c4 {

int Server::start(CmdInput &ci) {
  if (createListeningSock(ci) < 0) {
    return -1;
  }

  // kqueue
  int kqfd = kqueue();
  if (kqfd == -1) {
    errMsg = "can't initialize kqueue";
    return -1;
  }

  int chListCounter = 1;
  const int EVENT_LIST_COUNT = 1024;
  struct kevent chlist[EVENT_LIST_COUNT]; // events to monitor
  struct kevent evlist[EVENT_LIST_COUNT]; // events triggered

  // populate chlist
  EV_SET(&chlist[0], listenfd, EVFILT_READ, EV_ADD, 0, 0, 0);

  struct sockaddr_in cliaddr;
  const int READ_BUFFER_MAX = 1024;
  char readBuffer[READ_BUFFER_MAX];

  while (true) {
    int nev = kevent(kqfd, chlist, chListCounter, evlist, EVENT_LIST_COUNT, NULL);
    if (nev < 0) {
      errMsg = "kevent error";
      return -1;
    }

    // reset chlist
    chListCounter = 0;

    for (int i = 0; i < nev; i++) {
      // listening socket - accept new connections
      if (evlist[i].ident == (unsigned) listenfd) {
        // error
        if (evlist[i].flags & EV_ERROR) {
          errMsg = evlist[i].data;
          return -1;
        }

        socklen_t clilen = sizeof(cliaddr);
        int connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &clilen);
        if (connfd < 0) {
          std::cerr << "Failed to accept connection." << std::endl;
          continue;
        }

        // create message buffer
        createMessageBuffer(connfd);

        // monitor new connection
        EV_SET(&chlist[chListCounter++], connfd, EVFILT_READ | EVFILT_WRITE,
          EV_ADD, 0, 0, 0);
        continue;
      }

      // handle socket communication
      // read data
      if (evlist[i].flags & EVFILT_READ) {
        // evlist[i].data contains the number of bytes available
        int n = read(evlist[i].ident, readBuffer, READ_BUFFER_MAX);
        std::cout << "read " << n << " bytes" << std::endl;

        if (n < 0) {
          // TODO:
          // read error
        } else {
          // TODO:
          // send data to MessageBuffer
        }
      }

      // TODO:
      // write data

      // TODO:
      // evlist[i].flags & EV_EOF on shutdown
      // shutdown

      // TODO:
      // evlist[i].fflags contains socket error if any
    }
  }

  close(kqfd);
  return 0;
}

int Server::shutdown() {
  // TODO:
  return 0;
}

} // namespace

