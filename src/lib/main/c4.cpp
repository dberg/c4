#include <iostream>

#include "c4/main/CmdInput.h"

using namespace c4;
using std::cerr;
using std::endl;

int main(int argc, const char** argv) {
  CmdInput ci(argc, argv);
  if (ci.processCmdArgs()) {
    cerr << "Error: " << ci.getError() << endl;
    return 1;
  }


  return 0;
}
