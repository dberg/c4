#include "c4/server/Server.h"

namespace c4 {

int Server::start(unsigned int port) {
  if (createListeningSock(port) < 0) {
    log(LOG_ERROR, "Failed to create listening socket");
    return -1;
  }

  // kqueue
  int kqfd = kqueue();
  if (kqfd == -1) {
    log(LOG_ERROR, "Failed to initialize kqueue");
    return -1;
  }

  int chListCounter = 1;
  const int EVENT_LIST_COUNT = 256; // TODO: move configuration
  struct kevent chlist[EVENT_LIST_COUNT]; // events to monitor
  struct kevent evlist[EVENT_LIST_COUNT]; // events triggered

  // populate chlist
  EV_SET(&chlist[0], listenfd, EVFILT_READ, EV_ADD, 0, 0, 0);

  struct sockaddr_in cliaddr;

  while (true) {
    int nev = kevent(kqfd, chlist, chListCounter, evlist, EVENT_LIST_COUNT, NULL);
    if (nev < 0) {
      log(LOG_ERROR, "kevent error");
      return -1;
    }

    // reset chlist
    chListCounter = 0;

    for (int i = 0; i < nev; i++) {
      // listening socket - accept new connections
      if (evlist[i].ident == (unsigned) listenfd) {
        // error
        if (evlist[i].flags & EV_ERROR) {
          log(LOG_ERROR, "Error on listening socket. " + itos(evlist[i].data));
          return -1;
        }

        socklen_t clilen = sizeof(cliaddr);
        int connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &clilen);
        if (connfd < 0) {
          log(LOG_ERROR, "Failed to accept new connection");
          continue;
        }

        log(LOG_INFO, "New connection fd#" + itos(connfd));

        createRequestBuffer(connfd);

        // monitor new connection
        EV_SET(&chlist[chListCounter++], connfd, EVFILT_READ | EVFILT_WRITE,
          EV_ADD, 0, 0, 0);
        continue;
      }

      // handle socket communication
      // read data
      if (evlist[i].flags & EVFILT_READ) {
        // evlist[i].data also contains the number of bytes available
        int socket = evlist[i].ident;
        int cbytes = read(socket, readBuffer, READ_BUFFER_MAX);
        if (cbytes > 0) {
          // TODO: handle error when result is -1
          if (feed(socket, readBuffer, cbytes)) {
            // we have a complete request
            spRequest request = getRequest(socket);
            spResponse response = spResponse(new Response);
            projHandler->process(request, response);
            queueResponse(socket, response);
          }
        }

        // connection closed by the client
        // stop monitoring this socket
        if (cbytes < 0 || evlist[i].flags & EV_EOF) {
          close(evlist[i].ident);
        }
      }

      // write data
      if (evlist[i].flags & EVFILT_WRITE) {
        int socket = evlist[i].ident;
        writeResponses(socket);
      }
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

