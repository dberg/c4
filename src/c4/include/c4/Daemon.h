//-*- C++ -*-
#ifndef __DAEMON_H__
#define __DAEMON_H__
#include <iostream>
#include <netinet/in.h>     // sockaddr_in
#include <sys/socket.h>
#include <string>
#include <unistd.h>
#include "c4/Util.h"
#include "Config.h"
#include "CmdInput.h"

namespace c4 {

class Daemon {

  std::string errMsg;

public:
  Daemon() {}
  int start(CmdInput &ci);
  int shutdown();
  std::string getError() { return errMsg; }
};

} // namespace

#endif
