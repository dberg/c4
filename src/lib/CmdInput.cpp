#include "djp/CmdInput.h"

namespace djp {

int CmdInput::processCmdArgs() {
  if (argc < 2) {
    error = "Invalid filename";
    return 1;
  }

  filename = argv[1];
  return 0;
}

bool CmdInput::binaryFlag() {
  if (argc < 3) {
    return false;
  }

  std::string arg = argv[2];
  if (arg.compare("-b") == 0) {
    return true;
  }

  return false;
}

} // namespace
