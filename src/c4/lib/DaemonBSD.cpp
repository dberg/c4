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

  const int EVENTS_COUNT = 1;
  struct kevent chlist[EVENTS_COUNT]; // events to monitor
  struct kevent evlist[EVENTS_COUNT]; // events triggered

  // populate chlist
  EV_SET(&chlist[0], listenfd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);

  int nev, i;
  struct sockaddr_in cliaddr;
  while (true) {
    nev = kevent(kqfd, chlist, EVENTS_COUNT, evlist, EVENTS_COUNT, NULL);
    if (nev < 0) {
      errMsg = "kevent error";
      return -1;
    }

    for (i = 0; i < nev; i++) {
      // error
      if (evlist[i].flags & EV_ERROR) {
        errMsg = evlist[i].data;
        return -1;
      }

      // listening socket
      if (evlist[i].ident == (unsigned) listenfd) {
        socklen_t clilen = sizeof(cliaddr);
        int connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &clilen);
        if (connfd < 0) {
          errMsg = "accept connection error";
          return -1;
        }

        // TODO:
        // Handle connection
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

