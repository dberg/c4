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
  bool optHelp;
  bool optBinary;
  bool optEmacs;
  std::string filename;

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv),
      optHelp(false), optBinary(false), optEmacs(false), filename("") {}

  std::string help =
    "Usage:\n"
    "  djp [-h, --help] | [-b, --binary] | [--emacs] filename\n"
    "\n"
    "  --emacs          Parse java files\n"
    "  - b, --binary    Parse binary .class files\n"
    "  - h, --help      Show this help message\n"
    "\n"
    "Examples:\n"
    "\n"
    "  djp --emacs MyClass.java\n"
    "  djp -b MyClass.class\n";


  int processCmdArgs();
  bool isOptHelp() { return optHelp; }
  bool isOptBinary() { return optBinary; }
  bool isOptEmacs() { return optEmacs; }
  std::string getFilename() { return filename; }
  std::string getError() { return error; }
};

}
#endif
