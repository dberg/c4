//-*- C++ -*-
#ifndef __SCALA_INDENTATION_H__
#define __SCALA_INDENTATION_H__
#include "c4/ScalaToken.h"
#include <memory>
#include <map>

namespace c4 {

struct ScalaIndentation {
  int level;
  int lineWrap;
  STok token;
  int offset;
  ScalaIndentation(int level, int lineWrap, STok token, int offset)
    : level(level), lineWrap(lineWrap), token(token), offset(offset) {}
};

typedef std::shared_ptr<ScalaIndentation> spScalaIndentation;
typedef std::map<int, spScalaIndentation> ScalaLineIndentationMap;

void addIndentation(ScalaLineIndentationMap &indentMap, int line, int level,
  bool lineWrap, STok token, int offset = 0);

void addIndentationIfAbsent(ScalaLineIndentationMap &indentMap,
  int line, int level, bool lineWrap, STok token);

} // namespace

#endif
