#include <iostream>
#include <string>
#include "djp/CmdInput.h"
#include "djp/Diagnosis.h"
#include "djp/File.h"
#include "djp/Parser.h"
#include "djp/EmacsOutput.h"
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

  spDiagnosis diag = spDiagnosis(new Diagnosis);

  Parser parser(ci.filename, buffer, diag);
  parser.parse();
  if (parser.error) {
    std::cerr << "Error( " << parser.error << "): "
      << parser.error_msg << std::endl;
    return 1;
  }

  EmacsOutput output(parser.compilationUnit, parser.comments, parser.st, diag);
  output.build();
  std::cout << output.outSH << "\n" << output.outST;

  return 0;
}
