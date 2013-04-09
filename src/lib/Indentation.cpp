#include "djp/Indentation.h"

namespace djp {

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token) {
  indentMap[line] = spIndentation(new Indentation(level, lineWrap, token));
}

void addIndentationIfAbsent(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token) {
  LineIndentationMap::const_iterator it = indentMap.find(line);
  if (it != indentMap.end()) {
    return;
  }
  addIndentation(indentMap, line, level, lineWrap, token);
}

} // namespace
