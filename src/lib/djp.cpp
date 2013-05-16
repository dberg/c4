#include <iostream>
#include <string>
#include "djp/CmdInput.h"
#include "djp/File.h"
#include "djp/Parser.h"
#include "djp/ParserBin.h"
#include "djp/BinOutput.h"
#include "djp/EmacsOutput.h"
using namespace djp;

/*

Usage:
  djp [-b, --binary] | [--emacs] filename

  --emacs          Parse java files
  - b, --binary    Parse binary .class files

Examples:

  djp --emacs MyClass.java
  djp -b MyClass.class

*/

int parseJavaFile(CmdInput &ci) {
  std::string buffer;

  File file;
  if (file.read(ci.getFilename(), buffer)) {
    std::cerr << "Error: Failed to read file:" << ci.getFilename() << std::endl;
    return 1;
  }

  Parser parser(ci.getFilename(), buffer);
  parser.parse();
  if (parser.error) {
    std::cerr << "Error( " << parser.error << "): "
      << parser.error_msg << std::endl;
    return 1;
  }

  EmacsOutput output(parser);
  output.build();
  std::cout
    << output.outSH.str() << std::endl
    << output.outErr.str() << std::endl
    << output.outST.str() << std::endl
    << output.outIT.str();

  return 0;
}

int parseClassFile(CmdInput &ci) {
  std::vector<unsigned char> buffer;

  File file;
  if (file.read(ci.getFilename(), buffer)) {
    std::cerr << "Error: Failed to read file:" << ci.getFilename() << std::endl;
    return 1;
  }

  ParserBin parser(ci.getFilename(), buffer);
  parser.parse();

  BinOutput output(parser);
  output.build();
  std::cout << output.out.str() << std::endl;

  return 0;
}

int main(int argc, const char **argv) {
  CmdInput ci(argc, argv);
  if (ci.processCmdArgs()) {
    std::cerr << "Error: " << ci.getError() << std::endl;
    return 1;
  }

  if (ci.isOptBinary()) {
    return parseClassFile(ci);
  }

  return parseJavaFile(ci);
}
