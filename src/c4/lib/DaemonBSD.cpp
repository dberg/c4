#include "sys/event.h"
#include "c4/Daemon.h"

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
  int resl = listen(listenfd, LISTEN_BACKLOG);
  if (resl < 0) {
    errMsg = "can't listen on port ";
    errMsg += itos(ci.getPort());
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

