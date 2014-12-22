//-*- C++ -*-
#ifndef __C4_SCALA_SOURCE_FILE_H__
#define __C4_SCALA_SOURCE_FILE_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

using std::string;
using std::vector;

namespace c4s {

class SourceFile {
public:
  string filename;
  vector<Char> buffer;

  SourceFile(string filename, vector<Char> buffer);
};

} // namespace

#endif
