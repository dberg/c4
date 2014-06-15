//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <vector>

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

  /** a stack of tokens which indicates whether line-ends can be statement separators
   *  also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with `case'
   *  STRINGLIT if region is a string interpolation expression starting with '${'
   *            (the STRINGLIT appears twice in succession on the stack iff the
   *             expression is a multiline string literal).
   */
  std::vector<Token> sepRegions;

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
