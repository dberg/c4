//-*- C++ -*-
#ifndef __SCALA_INDENTATION_H__
#define __SCALA_INDENTATION_H__

#include <memory>
#include <map>
#include "c4/scala/Token.h"

namespace c4s {

struct Indentation {
  int level;
  int lineWrap;
  STok token;
  int offset;
  Indentation(int level, int lineWrap, STok token, int offset)
    : level(level), lineWrap(lineWrap), token(token), offset(offset) {}
};

typedef std::shared_ptr<Indentation> spIndentation;
typedef std::map<int, spIndentation> LineIndentationMap;

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, STok token, int offset = 0);

void addIndentationIfAbsent(LineIndentationMap &indentMap,
  int line, int level, bool lineWrap, STok token);

} // namespace

#endif
