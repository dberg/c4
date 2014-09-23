#include "c4/scala/SourceFile.h"

namespace c4s {

/** Constructor */
SourceFile::SourceFile() {}

/** Destructor */
SourceFile::~SourceFile() {}

/** Constructor */
ClientSourceFile::ClientSourceFile(
  std::string filename, std::vector<c4::Char> buffer)
  : SourceFile(), filename(filename), buffer(buffer) {}

/** Destructor */
ClientSourceFile::~ClientSourceFile() {}

} // namespace
