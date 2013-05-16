//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <string>

namespace djp {

class CmdInput {

  int argc;
  const char **argv;
  std::string error;

  // options
  bool optBinary;
  bool optEmacs;
  std::string filename;

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv), optBinary(false), optEmacs(false), filename("") {}

  int processCmdArgs();
  bool isOptBinary() { return optBinary; }
  bool isOptEmacs() { return optEmacs; }
  std::string getFilename() { return filename; }
  std::string getError() { return error; }
};

}
#endif
