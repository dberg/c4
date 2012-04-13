#include <iostream>
#include <string>
#include "CmdInput.h"
#include "File.h"
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

  std::cout << "Work in progress:" << std::endl << buffer;
  return 0;
}
