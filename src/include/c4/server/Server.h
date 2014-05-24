//-*- C++ -*-
#ifndef __SERVER_H__
#define __SERVER_H__

#include <netinet/in.h>     // sockaddr_in
#include <sys/socket.h>
#include <string>
#include <unistd.h>
#include <unordered_map>

#include "c4/main/Config.h"

// If we have epoll we assume linux and if we kqueue we assume a bsd system.
#ifdef HAVE_EPOLL
  #include <strings.h>      // bzero
  #include <sys/epoll.h>
#else
#ifdef HAVE_KQUEUE
  #include "sys/event.h"
#endif
#endif

#include "c4/common/ProjectHandler.h"
#include "c4/common/Log.h"
#include "c4/common/Util.h"
#include "c4/main/CmdInput.h"
#include "c4/server/Request.h"
#include "c4/server/Response.h"
#include "c4/server/RequestBuffer.h"

namespace c4 {

class Server {

public:

  Server() : projHandler(std::unique_ptr<ProjectHandler>(new ProjectHandler)) {}

  int start(unsigned int port);
  int shutdown();

private:

  std::unordered_map<int, spRequestBuffer> reqBuffers;
  std::unordered_map<int, std::vector<spResponse>> responses;

  int listenfd;

  spProjectHandler projHandler;

  /**
   * Create the listening socket that will accept incoming connections.
   */
  int createListeningSock(unsigned int port) {
    // create listening socket
    listenfd = socket(AF_INET, SOCK_STREAM, 0);
    if (listenfd < 0) {
      log(LOG_ERROR, "Failed to create listening socket");
      return -1;
    }

    struct sockaddr_in servaddr;
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(port);

    // bind
    int resb = bind(listenfd, (struct sockaddr *) &servaddr, sizeof(servaddr));
    if (resb < 0) {
      log(LOG_ERROR, "Failed to bind listening socket to port: " + itos(port));
      return -1;
    }

    // listen
    // TODO: a config file should optionally override this value
    const int LISTEN_BACKLOG = 1024;
    int resl = listen(listenfd, LISTEN_BACKLOG);
    if (resl < 0) {
      log(LOG_ERROR, "Failed to listen on port " + itos(port));
      return -1;
    }

    return 0;
  }

  /**
   * Create a RequestBuffer when a new connection is created.
   */
  int createRequestBuffer(int connfd) {
    if (reqBuffers.find(connfd) != reqBuffers.end()) {
      log(LOG_ERROR, "Existing RequestBuffer in fd#" + itos(connfd));
    }

    reqBuffers[connfd] = spRequestBuffer(new RequestBuffer);
    return 0;
  }

  /**
   * Add data to RequestBuffer
   */
  int feed(int connfd, char bytes[], int cbytes) {
    auto it = reqBuffers.find(connfd);

    // Invalid RequestBuffer.
    // We should abort the connection with this client.
    if (it == reqBuffers.end()) {
      log(LOG_ERROR, "Broken connection in fd#" + itos(connfd));
      return -1;
    }

    // pass data to RequestBuffer
    return it->second->feed(bytes, cbytes);
  }

  /**
   * Build a Request from a RequestBuffer.
   */
  spRequest getRequest(int connfd) {
    auto it = reqBuffers.find(connfd);
    if (it != reqBuffers.end()) {
      return it->second->buildAndRemoveRequest();
    }

    spRequest request = spRequest(new Request);
    return request;
  }

  /**
   * Write Responses to a socket.
   */
  void writeResponses(int socket) {
    // TODO: get the bytes of the message and keep track of what has been
    // written. See SerializeToOstream(ostream* output).

    auto it = responses.find(socket);
    if (it != responses.end()) {
      std::vector<spResponse> socketResponses = it->second;
      for (auto response : socketResponses) {
        log(LOG_INFO, "Writing response fd#" + itos(socket));
        response->SerializeToFileDescriptor(socket);
      }
    }
  }

};

} // namespace

#endif
