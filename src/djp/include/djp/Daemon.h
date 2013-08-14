//-*- C++ -*-
#ifndef __DAEMON_H__
#define __DAEMON_H__
#include <string>

namespace djp {

class Daemon {
  int argc;
  const char **argv;
  std::string error;

public:
  Daemon(int argc, const char **argv) : argc(argc), argv(argv) {}
  int start();
  int shutdown();
  std::string getError() { return error; }
};

} // namespace

#endif
