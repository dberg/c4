//-*- C++ -*-
#ifndef __C4_SCALA_SOURCE_FILE_H__
#define __C4_SCALA_SOURCE_FILE_H__

#include <memory>
#include <string>
#include <vector>

#include "c4/common/TypeDefs.h"

namespace c4s {

class SourceFile {
protected:
  virtual std::vector<c4::Char> content() = 0;

public:
  SourceFile() {}
};

/**
 * A source file sent over the wire.
 */
class ClientSourceFile : public SourceFile {

  std::string filename;
  std::vector<c4::Char> buffer;

public:
  ClientSourceFile(std::string filename, std::vector<c4::Char> buffer)
    : SourceFile(), filename(filename), buffer(buffer) {}

  std::vector<c4::Char> content() { return buffer; }
};

} // namespace

#endif
