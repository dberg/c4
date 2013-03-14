//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <string>

namespace djp {

class CmdInput {
public:
  int argc;
  const char **argv;

  std::string error;
  std::string filename;

  CmdInput(int argc, const char **argv) : argc(argc), argv(argv) {}
  int processCmdArgs();
  bool binaryFlag();
};

}
#endif
