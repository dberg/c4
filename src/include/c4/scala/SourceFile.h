//-*- C++ -*-
#ifndef __C4_SCALA_SOURCE_FILE_H__
#define __C4_SCALA_SOURCE_FILE_H__

#include <string>
#include <vector>
#include "c4/common/TypeDefs.h"

namespace c4s {

class SourceFile {
public:
  virtual std::vector<c4::Char>& content() = 0;
  SourceFile() {}
};

/**
 * A source file sent over the wire.
 */
class ClientSourceFile : public SourceFile {
public:
  std::string filename;
  std::vector<c4::Char> buffer;

  ClientSourceFile(std::string filename, std::vector<c4::Char> buffer);

  std::vector<c4::Char>& content() { return buffer; }
};

} // namespace

#endif
