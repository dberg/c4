//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <string>

namespace c4 {

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
  std::string port;

  int validateInput();

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv),
      optHelp(false), optDaemon(false), optInJava(false), optInScala(false),
      optInBytecode(false), optOutEmacs(false), filename("") {}

  std::string help =
    "Usage:\n"
    "  c4 [-h, --help] \n"
    "  c4 [-i, --input] INPUT_OPTIONS [-f, --filename] FILENAME\n"
    "  c4 [-d, --daemon] [-p, --port] PORT_NUMBER\n\n"
    "Where INPUT_OPTIONS is one of 'java', 'scala' or 'bytecode'.\n\n"
    "The output for 'java' or 'scala' input types is 'emacs'.\n"
    "The output for the input type 'bytecode' is plain text.\n\n"
    "Examples:\n\n"
    "  c4 -i java -f Foo.java\n"
    "  c4 -i scala -f Bar.scala\n"
    "  c4 --input bytecode --filename Baz.class\n"
    "  c4 --daemon --port 8000\n";

  int processCmdArgs();

  bool isOptHelp() { return optHelp; }
  bool isOptDaemon() { return optDaemon; }
  bool isOptInJava() { return optInJava; }
  bool isOptInScala() { return optInScala; }
  bool isOptInBytecode() { return optInBytecode; }

  std::string getFilename() { return filename; }
  std::string getPort() { return port; }
  std::string getError() { return error; }
};

}
#endif
