#include <iostream>
#include "CmdInput.h"
using namespace djp;

int main(int argc, const char **argv) {
  CmdInput ci;
  if (ci.processCmdArgs(argc, argv)) {
    std::cerr << "Error: " << ci.error << std::endl;
    return 1;
  }

  std::cout << "Work in progress" << std::endl;
  return 0;
}
