//-*- C++ -*-
#ifndef __C4_COMMON_COMPILATION_H__
#define __C4_COMMON_COMPILATION_H__

#include <memory>
#include <string>
#include <vector>

#include "c4/common/TypeDefs.h"
#include "utf8/utf8.h"

namespace c4 {

class Compilation;
typedef std::shared_ptr<Compilation> spCompilation;

/**
 * A Compilation contains the information extracted from a client Request to
 * parse a compilation unit and the output of this compilation.
 */
class Compilation {
public:

  /** Buffer filename */
  std::string filename;

  /** The vector buffer contains the compilation unit contents parsed as a Char. */
  // TODO: remove when the java parser uses Char
  std::string bufferStr;
  std::vector<Char> buffer;

  /**
   * The output of this compilation. The string contains elisp code that can be
   * parsed by the emacs mode c4-mode.
   */
  std::string output;

  Compilation(std::string filename, std::string bufferStr)
    : filename(filename), bufferStr(bufferStr) {

    // TODO: accept different encodings with a new Request flag.
    utf8::utf8to16(bufferStr.begin(), bufferStr.end(),
      std::back_inserter(buffer));
  }
};

} // namespace

#endif
