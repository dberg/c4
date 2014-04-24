#include <iostream>
#include <string>
#include "c4/bytecode/ParserBin.h"
#include "c4/bytecode/BinOutput.h"
#include "c4/common/SourceCodeStream.h"
#include "c4/java/Parser.h"
#include "c4/java/EmacsOutput.h"
#include "c4/scala/ScalaParser.h"
#include "c4/scala/ScalaEmacsOutput.h"
#include "c4/main/CmdInput.h"
#include "c4/main/File.h"
#include "c4/Server.h"

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

  c4j::Parser parser(ci.getFilename(), buffer);
  parser.parse();
  if (parser.error) {
    std::cerr << "Error( " << parser.error << "): "
      << parser.error_msg << std::endl;
    return 1;
  }

  c4j::EmacsOutput output(parser);
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

  c4s::ScalaParser parser(ci.getFilename(), buffer);
  parser.parse();

  c4s::ScalaEmacsOutput output(parser);
  output.build();
  std::cout << output.sh << std::endl
    << output.errors << std::endl
    << output.indentation;
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
  } else if (ci.isOptServer()) {
    Server server;
    if (server.start(ci.getServerPort()) < 0) {
      std::cerr << "FAILURE: " << server.getError() << std::endl;
      return 1;
    }
  }

  return 0;
}
