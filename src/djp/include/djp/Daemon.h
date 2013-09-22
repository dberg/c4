//-*- C++ -*-
#ifndef __DAEMON_H__
#define __DAEMON_H__
#include <string>
#include "CmdInput.h"

namespace djp {

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
