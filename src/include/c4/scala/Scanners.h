//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <vector>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/ScannersTypeDefs.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Tokens.h"

namespace c4s {

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

class Scanner: public TokenData, public ScannerData {

private:

  // SourceFile data
  spSourceFile source;
  std::vector<c4::Char> &buf;

  Offset offset;
  Offset lastOffset;

  spCharArrayReader r;

  void getIdentRest();
  void finishNamed(Token idToken = Token::T_IDENTIFIER);

protected:

  std::vector<c4::Char> cbuf;
  void putChar(c4::Char c);

public:

  Scanner(spSourceFile source)
    : source(source), buf(source->content()), offset(0), lastOffset(0),
      r(spCharArrayReader(new CharArrayReader(source->content()))) {}

  virtual bool inStringInterpolation();
  virtual void nextToken();
  virtual void fetchToken();
  virtual void init();
};

class UnitScanner : public Scanner {
private:
  spCompilationUnit unit;

public:
  UnitScanner(spCompilationUnit unit)
    : Scanner(unit->source), unit(unit) {}
};

} // namespace

#endif
