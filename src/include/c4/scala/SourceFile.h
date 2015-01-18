//-*- C++ -*-
#ifndef __C4S_SCALA_SOURCE_FILE_H__
#define __C4S_SCALA_SOURCE_FILE_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

using std::u32string;
using std::vector;

namespace c4s {

class SourceFile {
public:
  u32string filename;
  u32string buffer;

  SourceFile(u32string filename, u32string buffer);
};

} // namespace

#endif
