//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <string>

namespace djp {

class CmdInput {
public:
  std::string error;
  std::string filename;
  int processCmdArgs(int argc, const char **argv);
};

}
#endif
