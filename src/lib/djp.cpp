#include <iostream>
#include <string>
#include "djp/CmdInput.h"
#include "djp/File.h"
#include "djp/Parser.h"
#include "djp/ParserBin.h"
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
    << output.outSH << std::endl
    << output.outErr << std::endl
    << output.outST << std::endl
    << output.outIT;

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

  // TODO:
  std::cout << "I'm a happy binary file." << std::endl;
  std::cout << "Magic: " << parser.classFile->magic << std::endl;
  std::cout << "Minor: " << parser.classFile->minor_version << std::endl;
  std::cout << "Major: " << parser.classFile->major_version << std::endl;
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
