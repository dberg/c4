#include "c4/ScalaIndentation.h"

namespace c4 {

void addIndentation(ScalaLineIndentationMap &indentMap, int line, int level,
  bool lineWrap, STok token, int offset) {
  indentMap[line] = spScalaIndentation(new ScalaIndentation(
    level, lineWrap, token, offset));
}

void addIndentationIfAbsent(ScalaLineIndentationMap &indentMap,
  int line, int level, bool lineWrap, STok token) {
  ScalaLineIndentationMap::const_iterator it = indentMap.find(line);
  if (it != indentMap.end()) {
    return;
  }
  addIndentation(indentMap, line, level, lineWrap, token);
}

} // namespace
