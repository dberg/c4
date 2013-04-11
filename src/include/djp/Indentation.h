//-*- C++ -*-
#ifndef __INDENTATION_H__
#define __INDENTATION_H__
#include <map>
#include <boost/shared_ptr.hpp>

namespace djp {

struct Indentation {
  int level;
  int lineWrap;
  int token;
  int offset;
  Indentation(int level, int lineWrap, int token, int offset)
    : level(level), lineWrap(lineWrap), token(token), offset(offset) {}
};

typedef boost::shared_ptr<Indentation> spIndentation;
typedef std::map<int, spIndentation> LineIndentationMap;

void addIndentation(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token = 0, int offset = 0);

void addIndentationIfAbsent(LineIndentationMap &indentMap, int line, int level,
  bool lineWrap, int token = 0);

} // namespace

#endif
