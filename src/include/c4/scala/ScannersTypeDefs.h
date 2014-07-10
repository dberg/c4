//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_TYPE_DEFS_H__
#define __C4_SCALA_SCANNERS_TYPE_DEFS_H__

#include <memory>

namespace c4s {

typedef int Offset;

class TokenData;
typedef std::shared_ptr<TokenData> spTokenData;

class Scanner;
typedef std::shared_ptr<Scanner> spScanner;

class UnitScanner;
typedef std::shared_ptr<UnitScanner> spUnitScanner;

} // namespace

#endif
