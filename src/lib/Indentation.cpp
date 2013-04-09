#include "djp/Indentation.h"

namespace djp {

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token) {
  indentMap[line] = spIndentation(new Indentation(level, lineWrap, token));
}

} // namespace
