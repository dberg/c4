//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include <functional>
#include <string>
#include <vector>
#include "c4/scala/TypeDefs.h"
#include "c4/scala/Tokens.h"

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

  spPosition r2p(Offset start, Offset mid, Offset end);
  spPosition r2p(Offset start, Offset mid);
  spPosition r2p(Offset offset);

  //---------------------------------------------------------------------------
  // Token Classes
  //---------------------------------------------------------------------------
  bool isModifier();
  bool isAnnotation();
  bool isTemplateIntro();
  bool isIdent();
  bool isStatSep(Token token);
  bool isStatSep();

  //---------------------------------------------------------------------------
  // Tree construction
  //---------------------------------------------------------------------------
  spTree atPos(Offset offset, spTree t);
  spTree atPos(Offset offset, Offset point, spTree t);
  spTree atPos(spPosition pos, spTree t);

  //---------------------------------------------------------------------------
  // Identifiers and literals
  //---------------------------------------------------------------------------
  virtual spName ident(bool skipIt = true);
  virtual spTree selector(spTree t);
  virtual spTree selectors(spTree t, bool typeOK, Offset dotOffset);
  virtual spTree qualId();
  virtual spTree pkgQualId();

  virtual Offset caseAwareTokenOffset();
  virtual spTree topLevelTmpDef();
  virtual spTree packageOrPackageObject(Offset start);

  //---------------------------------------------------------------------------
  // STATSEQS
  //---------------------------------------------------------------------------
  virtual spTree makePackaging(Offset start, spTree pkg,
    std::vector<spTree> stats);
  virtual std::vector<spTree> topStatSeq();
  virtual std::function<std::vector<spTree> (Token)> topStat();

  virtual spTree compilationUnit();
  virtual std::vector<spTree> topstats();

public:
  Global *global;
  Parser(Global *global);
  virtual spTree parse();
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
