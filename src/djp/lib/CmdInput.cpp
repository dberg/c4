#include "djp/CmdInput.h"

namespace djp {

int CmdInput::processCmdArgs() {
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];

    // help option
    if (arg.compare("-h") == 0 || arg.compare("--help") == 0) {
      optHelp = true;
      return 0;
    }

    // input
    if (arg.compare("-i") == 0 || arg.compare("--input") == 0) {
      if (i + 1 >= argc) {
        error = "Missing input type.";
        return 1;
      }

      std::string arg = argv[++i];
      if (arg.compare("java") == 0) {
        optInJava = optOutEmacs = true;
      } else if (arg.compare("scala") == 0) {
        optInScala = optOutEmacs = true;
      } else if (arg.compare("bytecode") == 0) {
        optInBytecode = true;
      } else {
        error = "Invalid input option.";
        return 1;
      }

      continue;
    }

    if (arg.compare("-f") == 0 || arg.compare("--filename") == 0) {
      if (i + 1 >= argc) {
        error = "Missing filename.";
        return 1;
      }

      filename = argv[++i];
    }
  }

  return validateInput();
}

/**
 * Validate collected input options.
 * @return 0 if input is valid and 1 if it's invalid
 */
int CmdInput::validateInput() {
  // Filename is mandatory
  if (filename.empty()) {
    error = "Missing filename.";
    return 1;
  }

  bool result = optInJava || optInScala || optInBytecode;
  if (!result) {
    if (endsWith(filename, ".java")) {
      optInJava = optOutEmacs = true;
    } else if (endsWith(filename, ".scala")) {
      optInScala = optOutEmacs = true;
    } else if (endsWith(filename, ".class")) {
      optInBytecode = true;
    } else {
      error = "Unknown filename type.";
      return 1;
    }
  }

  return 0;
}

/**
 * Check if the string str ends with the string end.
 */
bool endsWith(std::string const &str, std::string const &end) {
  if (str.length() >= end.length()) {
    return (0 == str.compare(str.length() - end.length(), end.length(), end));
  }
  return false;
}

} // namespace
