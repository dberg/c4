//-*- C++ -*-
#ifndef __SERVER_H__
#define __SERVER_H__
#include <iostream>
#include <netinet/in.h>     // sockaddr_in
#include <sys/socket.h>
#include <string>
#include <unistd.h>
#include <unordered_map>

#include "Config.h"

// If we have epoll we assume linux and if we kqueue we assume a bsd system.
#ifdef HAVE_EPOLL
  #include <strings.h>      // bzero
  #include <sys/epoll.h>
#else
#ifdef HAVE_KQUEUE
  #include "sys/event.h"
#endif
#endif

#include "c4/Util.h"
#include "CmdInput.h"
#include "Message.h"

namespace c4 {

class Server {

  std::string errMsg;

  std::unordered_map<int, spMessage> rMessages;
  std::unordered_map<int, spMessage> wMessages;

public:
  Server() {}
  int start(CmdInput &ci);
  int shutdown();
  std::string getError() { return errMsg; }

private:

  int listenfd;

  int createListeningSock(CmdInput &ci) {
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
    // TODO: a config file should optionally override this value
    const int LISTEN_BACKLOG = 1024;
    int resl = listen(listenfd, LISTEN_BACKLOG);
    if (resl < 0) {
      errMsg = "can't listen on port ";
      errMsg += itos(ci.getPort());
      return -1;
    }

    return 0;
  }

};



} // namespace

#endif
