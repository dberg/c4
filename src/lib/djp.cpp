#include <iostream>
#include <string>
#include "CmdInput.h"
#include "File.h"
#include "Parser.h"
#include "Output.h"
using namespace djp;

int main(int argc, const char **argv) {
  CmdInput ci;
  if (ci.processCmdArgs(argc, argv)) {
    std::cerr << "Error: " << ci.error << std::endl;
    return 1;
  }

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

  Output output(parser.compilationUnit);
  output.print();

  return 0;
}
