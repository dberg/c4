//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <vector>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/ScannersTypeDefs.h"
#include "c4/scala/Tokens.h"

namespace c4s {

//class ScannerCommon {};

class TokenData {

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
  Token token;
  TokenData() : token(Token::T_EMPTY) {}
};

class ScannerData {
protected:
  spTokenData next;
  spTokenData prev;

public:
  ScannerData()
    : next(spTokenData(new TokenData)), prev(spTokenData(new TokenData)) {}
};

class Scanner : public CharArrayReader,
                public TokenData,
                public ScannerData //,
                /*public ScannerCommon*/ {

  Offset offset;
  Offset lastOffset;

public:

  Scanner() : TokenData(), ScannerData(), offset(0), lastOffset(0) {}

  virtual void nextToken();
  virtual void fetchToken();
  virtual void init();
};

class UnitScanner : public Scanner {
private:
  spCompilationUnit unit;

public:
  UnitScanner(spCompilationUnit unit): Scanner(), unit(unit) {}
};

} // namespace

#endif
