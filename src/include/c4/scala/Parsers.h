//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"

namespace c4s {

class Parser {
private:
  // Set for files that start with package scala
  bool inScalaPackage;
  std::string currentPackage;
  void resetPackage() {
    inScalaPackage = false;
    currentPackage = "";
  }

protected:
  spScanner in;

  // TODO:
  //spPosition r2p(Offset start, Offset mid, Offset end);
  //spPosition r2p(Offset start, Offset mid);
  //spPosition r2p(Offset offset);

  //---------------------------------------------------------------------------
  // Token Classes
  //---------------------------------------------------------------------------
  bool isIdent();

  //---------------------------------------------------------------------------
  // Tree construction
  //---------------------------------------------------------------------------
  spTree atPos(Offset offset, spTree t);

  //---------------------------------------------------------------------------
  // Identifiers and literals
  //---------------------------------------------------------------------------
  virtual spName ident(bool skipIt = true);
  virtual spTree qualId();
  virtual spTree pkgQualId();

  virtual std::vector<spTree> topstats();

public:
  Global *global;
  Parser(Global *global);
  virtual spTree parse();
  virtual spTree compilationUnit();
};

class SourceFileParser : public Parser {
public:
  SourceFileParser(Global *global);
};

class UnitParser : public SourceFileParser {

private:
  spCompilationUnit unit;

public:
  UnitParser(Global *global, spCompilationUnit unit);
};

} // namespace

#endif
