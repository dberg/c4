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

        // Set file descriptor to non-blocking
        fcntl(connfd, F_SETFL, O_NONBLOCK);

        log(LOG_INFO, "New connection fd#" + itos(connfd));

        createSocketBuffers(connfd);

        // monitor new connection
        EV_SET(&chlist[chListCounter++], connfd, EVFILT_READ, EV_ADD, 0, 0, 0);
        continue;
      }

      // Write data
      if (evlist[i].filter == EVFILT_WRITE) {
        int socket = evlist[i].ident;
        int result = writeResponses(socket);
        if (result < 0) {
          // TODO: handle error case.
          log(LOG_ERROR, "Failed to write to fd#" + itos(socket));
        } else if (result == 0) {
          // No more data to be written, stop listening for writes
          log(LOG_INFO, "All data written. Disabling writes on fd#" + itos(socket));
          EV_SET(&chlist[chListCounter++], socket,
            EVFILT_WRITE,
            EV_DISABLE, 0, 0, 0);
        }

        // If result is positive we keep listening for writes.
      }

      // handle socket communication
      // read data
      if (evlist[i].filter == EVFILT_READ) {
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
            // Add EVFILT_WRITE since we now have data to be written
            log(LOG_INFO, "add write filter to fd#" + itos(socket));
            EV_SET(&chlist[chListCounter++], socket, EVFILT_WRITE, EV_ADD, 0, 0, 0);
          }
        }

        // connection closed by the client
        // stop monitoring this socket
        if (cbytes < 0 || evlist[i].flags & EV_EOF) {
          log(LOG_INFO, "closing fd#" + itos(evlist[i].ident));
          close(evlist[i].ident);
        }
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
