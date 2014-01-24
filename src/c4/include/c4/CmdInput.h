//-*- C++ -*-
#ifndef __CMD_INPUT_H__
#define __CMD_INPUT_H__
#include <sstream>
#include <string>

namespace c4 {

bool endsWith(std::string const &str, std::string const &end);

class CmdInput {

  int argc;
  const char **argv;
  std::string error;

  // options
  bool optHelp;
  bool optServer;
  bool optInJava;
  bool optInScala;
  bool optInBytecode;
  bool optOutEmacs;
  std::string filename;
  unsigned int port;

  int validateInput();

public:

  CmdInput(int argc, const char **argv)
    : argc(argc), argv(argv),
      optHelp(false), optServer(false), optInJava(false), optInScala(false),
      optInBytecode(false), optOutEmacs(false), filename(""), port(0) {}

  std::string help =
    "Usage:\n"
    "  c4 [-h, --help] \n"
    "  c4 [-i, --input] INPUT_OPTIONS [-f, --filename] FILENAME\n"
    "  c4 [-s, --server] [-p, --port] PORT_NUMBER\n\n"
    "Where INPUT_OPTIONS is one of 'java', 'scala' or 'bytecode'.\n\n"
    "The output for 'java' or 'scala' input types is 'emacs'.\n"
    "The output for the input type 'bytecode' is plain text.\n\n"
    "Examples:\n\n"
    "  c4 -i java -f Foo.java\n"
    "  c4 -i scala -f Bar.scala\n"
    "  c4 --input bytecode --filename Baz.class\n"
    "  c4 --server --port 8000\n";

  int processCmdArgs();

  bool isOptHelp() { return optHelp; }
  bool isOptServer() { return optServer; }
  bool isOptInJava() { return optInJava; }
  bool isOptInScala() { return optInScala; }
  bool isOptInBytecode() { return optInBytecode; }

  std::string getFilename() { return filename; }
  unsigned int getPort() { return port; }
  std::string getError() { return error; }
};

}
#endif
