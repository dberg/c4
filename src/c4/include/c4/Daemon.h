//-*- C++ -*-
#ifndef __DAEMON_H__
#define __DAEMON_H__
#include <string>
#include "CmdInput.h"

namespace c4 {

class Daemon {

  std::string errorMsg;

public:
  Daemon() {}
  int start(CmdInput &ci);
  int shutdown();
  std::string getError() { return errorMsg; }
};

} // namespace

#endif
