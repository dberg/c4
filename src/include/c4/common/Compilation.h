//-*- C++ -*-
#ifndef __C4_COMMON_COMPILATION_H__
#define __C4_COMMON_COMPILATION_H__

#include <memory>
#include <string>
#include <vector>

using std::u32string;

namespace c4 {

class Compilation;
typedef std::shared_ptr<Compilation> spCompilation;

/**
 * A Compilation contains the information extracted from a client Request to
 * parse a compilation unit and the output of this compilation.
 */
class Compilation {
public:

  u32string filename;
  u32string buffer;

  /**
   * The output of this compilation. The string contains elisp code that can be
   * parsed by the emacs mode c4-mode.
   */
  u32string output;

  Compilation(u32string filename, u32string buffer)
    : filename(filename), buffer(buffer) {}
};

} // namespace

#endif
