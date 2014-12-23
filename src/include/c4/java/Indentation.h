//-*- C++ -*-
#ifndef __C4_JAVA_INDENTATION_H__
#define __C4_JAVA_INDENTATION_H__

#include <memory>
#include <map>

using std::map;
using std::shared_ptr;

namespace c4j {

struct Indentation {
  int level;
  int lineWrap;
  int token;
  int offset;
  Indentation(int level, int lineWrap, int token, int offset)
    : level(level), lineWrap(lineWrap), token(token), offset(offset) {}
};

typedef shared_ptr<Indentation> spIndentation;
typedef map<int, spIndentation> LineIndentationMap;

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token = 0, int offset = 0);

void addIndentationIfAbsent(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token = 0);

} // namespace

#endif
