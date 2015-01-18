#include <iostream>
#include <string>

#include "c4/common/Encode.h"
#include "c4/bytecode/ParserBin.h"
#include "c4/bytecode/BinOutput.h"
#include "c4/java/EmacsOutput.h"
#include "c4/java/Parser.h"
#include "c4/main/CmdInput.h"
#include "c4/main/File.h"
#include "c4/server/Server.h"

using namespace c4;
using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::u32string;
using std::vector;

int parseClassFile(CmdInput &ci) {
  vector<unsigned char> buffer;

  File file;
  if (file.read(ci.getFilename(), buffer)) {
    cerr << "Error: Failed to read file:" << ci.getFilename() << endl;
    return 1;
  }

  ParserBin parser(ci.getFilename(), buffer);
  parser.parse();

  BinOutput output(parser);
  output.build();
  cout << output.out.str() << endl;

  return 0;
}

int parseJavaFile(CmdInput &ci) {
  string buffer;
  File file;
  if (file.read(ci.getFilename(), buffer)) {
    cerr << "Error: Failed to read file:" << ci.getFilename() << endl;
    return 1;
  }

  u32string u32buffer = utf8_to_u32(buffer);
  u32string u32filename = utf8_to_u32(ci.getFilename());

  c4j::Parser parser(u32buffer, u32filename);
  parser.parse();
  if (parser.error) {
    string error_msg = u32_to_utf8(parser.error_msg);
    cerr << "Error( " << parser.error << "): "
      << error_msg << endl;
    return 1;
  }

  c4j::EmacsOutput emacs(parser);
  emacs.build();
  string output = u32_to_utf8(emacs.body());
  cout << output;

  return 0;
}

int parseScalaFile(CmdInput &ci) {
  string buffer;

  File file;
  if (file.read(ci.getFilename(), buffer)) {
    cerr << "Error: Failed to read file:" << ci.getFilename() << endl;
    return 1;
  }

  // TODO: use c4s::Global
  //c4s::Parser parser(ci.getFilename(), buffer);
  //parser.parse();

  // TODO: refactor EmacsOutput based on Global
  //c4s::EmacsOutput output(parser);
  //output.build();
  //std::cout << output.body();

  return 0;
}

int main(int argc, const char **argv) {
  CmdInput ci(argc, argv);
  if (ci.processCmdArgs()) {
    cerr << "Error: " << ci.getError() << endl;
    return 1;
  }

  if (ci.isOptHelp()) {
    cout << ci.help << endl;
  } else if (ci.isOptInBytecode()) {
    return parseClassFile(ci);
  } else if (ci.isOptInJava()) {
    return parseJavaFile(ci);
  } else if (ci.isOptInScala()) {
    return parseScalaFile(ci);
  } else if (ci.isOptServer()) {
    Server server;
    if (server.start(ci.getServerPort()) < 0) {
      return 1;
    }
  }

  return 0;
}
