//-*- C++ -*-
#ifndef __C4_CMD_INPUT_H__
#define __C4_CMD_INPUT_H__

#include <sstream>
#include <string>
#include "c4/common/Util.h"

using std::string;

namespace c4 {

class CmdInput {

  int argc;
  const char **argv;
  string error;

  // options
  bool optHelp;
  bool optServer;
  bool optInJava;
  bool optInBytecode;
  bool optOutEmacs;
  string filename;
  unsigned int port;

  int validateInput();

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv),
      optHelp(false), optServer(false), optInJava(false),
      optInBytecode(false), optOutEmacs(false), filename(""), port(0) {}

  string help =
    "Usage:\n"
    "  c4 [-h, --help] \n"
    "  c4 [-i, --input] INPUT_OPTIONS [-f, --filename] FILENAME\n"
    "  c4 [-s, --server] [-p, --port] PORT_NUMBER\n\n"
    "Where INPUT_OPTIONS is 'java' or 'bytecode'.\n\n"
    "The output for the 'java' input type is 'emacs'.\n"
    "The output for the 'bytecode' input type is plain text.\n\n"
    "Examples:\n\n"
    "  c4 -i java -f Foo.java\n"
    "  c4 --input bytecode --filename Baz.class\n"
    "  c4 --server --port 8000\n";

  int processCmdArgs();

  bool isOptHelp() { return optHelp; }
  bool isOptServer() { return optServer; }
  bool isOptInJava() { return optInJava; }
  bool isOptInBytecode() { return optInBytecode; }

  string getFilename() { return filename; }
  unsigned int getServerPort() { return port; }
  string getError() { return error; }
};

}
#endif
