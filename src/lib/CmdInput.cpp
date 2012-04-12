#include "CmdInput.h"

namespace djp {

int CmdInput::processCmdArgs(int argc, const char **argv) {
  if (argc < 2) {
    error = "Invalid filename";
    return 1;
  }

  filename = argv[1];

  return 0;
}
}
