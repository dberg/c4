#include "c4/scala/SourceFile.h"

namespace c4s {

/** Constructor */
ClientSourceFile::ClientSourceFile(
  std::string filename, std::vector<c4::Char> buffer)
  : SourceFile(), filename(filename), buffer(buffer) {}

} // namespace
