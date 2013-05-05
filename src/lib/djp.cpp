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
  djp filename [-b]

Where filename can be a java class (ex.: MyClass.java) or a binary class file
(ex.: MyClass.class). If the file is a binary class file the parameter '-b' is
mandatory.

Examples:

  djp MyClass.java
  djp MyClass.class -b

*/

int parseJavaFile(CmdInput &ci) {
  std::string buffer;

  File file;
  if (file.read(ci.filename, buffer)) {
    std::cerr << "Error: Failed to read file:" << ci.filename << std::endl;
    return 1;
  }

  Parser parser(ci.filename, buffer);
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
  if (file.read(ci.filename, buffer)) {
    std::cerr << "Error: Failed to read file:" << ci.filename << std::endl;
    return 1;
  }

  ParserBin parser(ci.filename, buffer);
  parser.parse();

  BinOutput output(parser);
  output.build();
  std::cout << output.out.str() << std::endl;

  return 0;
}

int main(int argc, const char **argv) {
  CmdInput ci(argc, argv);
  if (ci.processCmdArgs()) {
    std::cerr << "Error: " << ci.error << std::endl;
    return 1;
  }

  // Flag -b
  if (ci.binaryFlag()) {
    return parseClassFile(ci);
  }

  return parseJavaFile(ci);
}
