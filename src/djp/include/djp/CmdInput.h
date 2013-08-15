//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <string>

namespace djp {

bool endsWith(std::string const &str, std::string const &end);

class CmdInput {

  int argc;
  const char **argv;
  std::string error;

  // options
  bool optHelp;
  bool optDaemon;
  bool optInJava;
  bool optInScala;
  bool optInBytecode;
  bool optOutEmacs;
  std::string filename;

  int validateInput();

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv),
      optHelp(false), optInJava(false), optInScala(false), optInBytecode(false),
      optOutEmacs(false), filename("") {}

  std::string help =
    "Usage:\n"
    "  djp [-h, --help] \n"
    "  djp [-i, --input] INPUT_OPTIONS [-f, --filename] FILENAME\n"
    "  djp [-d, --daemon] [-p, --port] "
    "PORT_NUMBER [-f, --filename] FILENAME\n\n"
    "Where INPUT_OPTIONS is one of 'java', 'scala' or 'bytecode'.\n\n"
    "The output for 'java' and 'scala' inputs is 'emacs'. The output for the \n"
    "input type 'bytecode' is plain text.\n\n"
    "Examples:\n\n"
    "  djp -i java -f Foo.java\n"
    "  djp -i scala -f Bar.scala\n"
    "  djp --input bytecode --filename Baz.class\n"
    "  djp --daemon --port 8000 --filename /home/username/project\n";

  int processCmdArgs();

  bool isOptHelp() { return optHelp; }
  bool isOptDaemon() { return optDaemon; }
  bool isOptInJava() { return optInJava; }
  bool isOptInScala() { return optInScala; }
  bool isOptInBytecode() { return optInBytecode; }

  std::string getFilename() { return filename; }
  std::string getError() { return error; }
};

}
#endif
