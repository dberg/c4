//-*- C++ -*-
#ifndef __INDENTATION_H__
#define __INDENTATION_H__
#include <map>
#include <boost/shared_ptr.hpp>

namespace djp {

struct Indentation {
  int level;
  int offset;
  Indentation(int level, int offset) : level(level), offset(offset) {}
};

typedef boost::shared_ptr<Indentation> spIndentation;
typedef std::map<int, spIndentation> LineIndentationMap;

} // namespace

#endif
