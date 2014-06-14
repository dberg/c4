//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include "c4/scala/ScannersTypeDefs.h"
#include "c4/scala/Tokens.h"

namespace c4s {

/* TODO Notes:

CommonTokenData
ScannerCommon

TokenData, CharArrayReaderData
ScannerData

CommonTokenData

CommonTokenData
TokenData

CharArrayReader, TokenData, ScannerData, ScannerCommon
Scanner
SourceFileScanner
UnitScanner

*/

class TokenData {
protected:
  Token token;
public:
  TokenData() : token(EMPTY) {}
};

class Scanner : public TokenData {
protected:
  virtual void nextToken();
};

class UnitScanner : public Scanner {

};

} // namespace

#endif
