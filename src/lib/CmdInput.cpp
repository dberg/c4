#include "djp/CmdInput.h"

namespace djp {

int CmdInput::processCmdArgs() {
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg.compare("-b") == 0 || arg.compare("--binary") == 0) {
      optBinary = true;
      continue;
    }

    if (arg.compare("--emacs") == 0) {
      optEmacs = true;
      continue;
    }

    if (arg[0] == '-') {
      error = "Invalid option: " + arg;
      return 1;
    }

    if (filename.size() > 0) {
      error = "More than one filename provided.";
      return 1;
    }

    filename = argv[i];
  }

  if (filename.size() == 0) {
    error = "No filename provided.";
    return 1;
  }

  if (!optBinary && !optEmacs) {
    // Default to emacs
    optEmacs = true;
  }

  return 0;
}
} // namespace
