#include "c4/scala/SourceFile.h"

namespace c4s {

/** Constructor */
SourceFile::SourceFile(string filename, vector<Char> buffer)
  : filename(filename), buffer(buffer) {}

} // namespace
