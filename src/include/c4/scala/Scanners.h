//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <vector>

#include "c4/scala/CharArrayReader.h"
#include "c4/scala/ScannersTypeDefs.h"
#include "c4/scala/Tokens.h"

namespace c4s {

class ScannerData {
protected:
  spTokenData next;
  spTokenData prev;

  ScannerData()
    : next(spTokenData(new TokenData)), prev(spTokenData(new TokenData))
  {}
};

class TokenData {

protected:

  Token token;

  /** A stack of tokens which indicates whether line-ends can be statement
   *  separators. It's also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with `case'
   *  STRINGLIT if region is a string interpolation expression starting with
   *            '${' (the STRINGLIT appears twice in succession on the stack if
   *             the expression is a multiline string literal).
   */
  std::vector<Token> sepRegions;

public:
  TokenData() : token(EMPTY) {}
};

class Scanner : public CharArrayReader,
                public TokenData,
                public ScannerData,
                public ScannerCommon {
protected:

  Offset offset;
  Offset lastOffset;

  virtual void nextToken();
  virtual void fetchToken();
  virtual void init();

public:
  Scanner() : offset(0), lastOffset(0) {}
};

class UnitScanner : public Scanner {

};

} // namespace

#endif
