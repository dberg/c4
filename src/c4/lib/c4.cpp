#include <iostream>
#include <string>
#include "c4/SourceCodeStream.h"
#include "c4/CmdInput.h"
#include "c4/Daemon.h"
#include "c4/File.h"
#include "c4/Parser.h"
#include "c4/EmacsOutput.h"
#include "c4/ScalaParser.h"
#include "c4/ScalaEmacsOutput.h"
#include "c4/ParserBin.h"
#include "c4/BinOutput.h"

using namespace c4;

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

int parseScalaFile(CmdInput &ci) {
  std::string buffer;

  File file;
  if (file.read(ci.getFilename(), buffer)) {
    std::cerr << "Error: Failed to read file:" << ci.getFilename() << std::endl;
    return 1;
  }

  ScalaParser parser(ci.getFilename(), buffer);
  parser.parse();

  ScalaEmacsOutput output(parser.compUnit, parser.diag, parser.indentMap);
  output.build();
  /*
  std::cout
    << output.outSH.str() << std::endl
    << output.outErr.str() << std::endl
    << output.outST.str() << std::endl
    << output.outIT.str();
  */

  return 0;
}

int main(int argc, const char **argv) {
  CmdInput ci(argc, argv);
  if (ci.processCmdArgs()) {
    std::cerr << "Error: " << ci.getError() << std::endl;
    return 1;
  }

  if (ci.isOptHelp()) {
    std::cout << ci.help << std::endl;
  } else if (ci.isOptInBytecode()) {
    return parseClassFile(ci);
  } else if (ci.isOptInJava()) {
    return parseJavaFile(ci);
  } else if (ci.isOptInScala()) {
    return parseScalaFile(ci);
  } else if (ci.isOptDaemon()) {
    Daemon daemon;
    daemon.start(ci);
  }

  return 0;
}
