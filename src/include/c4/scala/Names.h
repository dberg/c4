//-*- C++ -*-
#ifndef __C4_SCALA_NAMES_H__
#define __C4_SCALA_NAMES_H__

#include <string>
#include <vector>

#include "c4/common/TypeDefs.h"

namespace c4s {

class Names {
public:

  spTermName newTermName(std::vector<c4::Char> cs, int offset, int len,
    std::string cachedString = "");
};

// TODO:
class Name {

};

// TODO:
class TermName : public Name {};

} // namespace

#endif
