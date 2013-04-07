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
  Indentation(int level, int lineWrap, int token)
    : level(level), lineWrap(lineWrap), token(token) {}
};

typedef boost::shared_ptr<Indentation> spIndentation;
typedef std::map<int, spIndentation> LineIndentationMap;

} // namespace

#endif
