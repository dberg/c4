#include "c4/scala/SourceFile.h"

namespace c4s {

/** Constructor */
SourceFile::SourceFile(u32string filename, u32string buffer)
  : filename(filename), buffer(buffer) {}

} // namespace
