#include "c4/Daemon.h"

namespace c4 {

int Daemon::start(CmdInput &ci) {
  if (createListeningSock(ci) < 0) {
    return -1;
  }

  // kqueue
  int kqfd = kqueue();
  if (kqfd == -1) {
    errMsg = "can't initialize kqueue";
    return -1;
  }

  int chlistCounter = 1;
  const int EVENT_LIST_COUNT = 1024;
  struct kevent chlist[EVENTS_COUNT]; // events to monitor
  struct kevent evlist[EVENTS_COUNT]; // events triggered

  // populate chlist
  EV_SET(&chlist[0], listenfd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);

  int nev, i, monitorIdx = 0;
  struct sockaddr_in cliaddr;
  while (true) {
    nev = kevent(kqfd, chlist, chlistCounter, evlist, EVENTS_COUNT, NULL);
    if (nev < 0) {
      errMsg = "kevent error";
      return -1;
    }

    // reset chlist
    chlist = 0;

    for (i = 0; i < nev; i++) {
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
          // TODO: log error
          continue;
        }

        // monitor new connection
        chlistCounter++;
        EV_SET(&chlist[chlistCounter], connfd, EFILT_READ | EFILT_WRITE,
          EV_ADD, 0, 0, 0)
        continue;
      }

      // handle socket communication
      // evlist[i].data contains the number of bytes available
      // evlist[i].flags & EV_EOF on shutdown
      // evlist[i].fflags contains socket error if any
      int n = read(evlist[i].ident, readBuffer, READ_BUFFER_MAX);
      if (n < 0) {
        // TODO:
        // read error
      } else if (n == 0) {
        // TODO:
        // No more data coming in. Do we actually have to handle that?
      } else {
        // TODO:
        // data is available
      }
    }
  }

  close(kqfd);
  return 0;
}

int Daemon::shutdown() {
  // TODO:
  return 0;
}

} // namespace

