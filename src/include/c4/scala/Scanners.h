//-*- C++ -*-
#ifndef __C4_SCALA_SCANNERS_H__
#define __C4_SCALA_SCANNERS_H__

#include <vector>

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CharArrayReader.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/SourceFile.h"
#include "c4/scala/Tokens.h"

namespace c4s {

class TokenData {

public:

  /** The next token */
  Token token;

  /** The offset of the first character of the current token */
  Offset offset;

  /** The offset of the character following the token preceding this one */
  Offset lastOffset;

  /** The name of an identifier */
  spTermName name;

  /** The string value of a literal */
  std::string strVal;

  /** The base of a number */
  int base;

  TokenData() : token(Token::T_EMPTY), offset(0), lastOffset(0), base(0) {}
};

class ScannerData {
public:
  spTokenData next;
  spTokenData prev;

  ScannerData()
    : next(spTokenData(new TokenData)), prev(spTokenData(new TokenData)) {}
};

class Scanner {
private:

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

  // SourceFile data
  spSourceFile source;
  std::vector<c4::Char> &buf;

  spCharArrayReader reader;
  spScannerData sData;
  spTokenData tData;

  void getIdentRest();
  void finishNamed(Token idToken = Token::T_IDENTIFIER);

protected:

  std::vector<c4::Char> cbuf;
  void putChar(c4::Char c);

public:

  Global *global;

  Scanner(Global *global, spSourceFile source)
    : source(source), buf(source->content()),
      reader(spCharArrayReader(new CharArrayReader(source->content()))),
      sData(spScannerData(new ScannerData)),
      tData(spTokenData(new TokenData)),
      global(global) {}

  virtual bool inStringInterpolation();
  virtual void nextToken();
  virtual void fetchToken();
  virtual void init();
};

class UnitScanner : public Scanner {
private:
  spCompilationUnit unit;

public:
  UnitScanner(Global *global, spCompilationUnit unit)
    : Scanner(global, unit->source), unit(unit) {}
};

} // namespace

#endif
