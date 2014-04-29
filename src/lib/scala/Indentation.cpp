#include "c4/scala/Indentation.h"

namespace c4s {

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, STok token, int offset) {
  indentMap[line] = spIndentation(new Indentation(
    level, lineWrap, token, offset));
}

void addIndentationIfAbsent(LineIndentationMap &indentMap,
  int line, int level, bool lineWrap, STok token) {
  LineIndentationMap::const_iterator it = indentMap.find(line);
  if (it != indentMap.end()) {
    return;
  }
  addIndentation(indentMap, line, level, lineWrap, token);
}

} // namespace
