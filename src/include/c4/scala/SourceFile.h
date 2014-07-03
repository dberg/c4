//-*- C++ -*-
#ifndef __C4_SCALA_CHAR_ARRAY_READER_H__
#define __C4_SCALA_CHAR_ARRAY_READER_H__

#include <memory>
#include <string>
#include <vector>

#include "c4/common/TypeDefs.h"
using c4::Char;

namespace c4s {

class SourceFile;
typedef std::shared_ptr<SourceFile> spSourceFile;

class ClientSourceFile;
typedef std::shared_ptr<ClientSourceFile> spClientSourceFile;

class SourceFile {
  virtual std::vector<Char> content() = 0;
};

/**
 * A source file sent over the wire.
 */
class ClientSourceFile : public SourceFile {

  std::string filename;
  std::vector<Char> buffer;

public:
  ClientSourceFile(std::string filename, std::vector<Char> buffer)
    : filename(filename), buffer(buffer) {}

  std::vector<Char> content() { return buffer; }
};

} // namespace

#endif
