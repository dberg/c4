#include "c4/main/CmdInput.h"

namespace c4 {

int CmdInput::processCmdArgs() {
  for (int i = 1; i < argc; i++) {
    string arg = argv[i];

    // help option
    if (arg.compare("-h") == 0 || arg.compare("--help") == 0) {
      optHelp = true;
      return 0;
    }

    // input type
    if (arg.compare("-i") == 0 || arg.compare("--input") == 0) {
      if (i + 1 >= argc) {
        error = "Missing input type.";
        return 1;
      }

      string arg = argv[++i];
      if (arg.compare("java") == 0) {
        optInJava = optOutEmacs = true;
      } else if (arg.compare("bytecode") == 0) {
        optInBytecode = true;
      } else {
        error = "Invalid input option.";
        return 1;
      }

      continue;
    }

    // filename path
    if (arg.compare("-f") == 0 || arg.compare("--filename") == 0) {
      if (i + 1 >= argc) {
        error = "Missing filename.";
        return 1;
      }

      filename = argv[++i];
    }

    // server
    if (arg.compare("-s") == 0 || arg.compare("--server") == 0) {
      optServer = true;
      // Filename should point to the c4 project file. ".c4"
      optOutEmacs = true;
      continue;
    }

    // port number
    if (arg.compare("-p") == 0 || arg.compare("--port") == 0) {
      if (i + 1 >= argc) {
        error = "Missing port number.";
        return 1;
      }

      string portStr = argv[++i];
      std::stringstream ss(portStr);
      if (!(ss >> port)) {
        error = "Invalid port number.";
        return 1;
      }
    }

  }

  return validateInput();
}

/**
 * Validate collected input options.
 * @return 0 if input is valid and 1 if it's invalid
 */
int CmdInput::validateInput() {
  // Filename is mandatory if not in deamon mode
  if (filename.empty() && !optServer) {
    error = "Missing filename.";
    return 1;
  }

  if (port <= 0 && optServer) {
    error = "Missing port number.";
    return 1;
  }

  // If we don't have the input type, and we don't have the server option
  // we try to infer the output type based in the filename extension.
  bool optIn = optInJava || optInBytecode;
  if (!optIn && !optServer) {
    if (endsWith<string>(filename, ".java")) {
      optInJava = optOutEmacs = true;
    } else if (endsWith<string>(filename, ".class")) {
      optInBytecode = true;
    } else {
      error = "Unknown filename type.";
      return 1;
    }
  }

  return 0;
}

} // namespace
