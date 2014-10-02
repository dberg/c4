//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include <functional>
#include <string>
#include <vector>
#include "c4/scala/CompilationUnits.h"
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
  Scanner *in;

  Position* r2p(Offset start, Offset mid, Offset end);
  Position* r2p(Offset start, Offset mid);
  Position* r2p(Offset offset);

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
  Tree* atPos(Offset offset, Tree* t);
  Tree* atPos(Offset offset, Offset point, Tree* t);
  Tree* atPos(Position* pos, Tree* t);

  //---------------------------------------------------------------------------
  // Identifiers and literals
  //---------------------------------------------------------------------------
  virtual Name* ident(bool skipIt = true);
  virtual Tree* selector(Tree* t);
  virtual Tree* selectors(Tree* t, bool typeOK, Offset dotOffset);
  virtual Tree* qualId();
  virtual Tree* pkgQualId();

  virtual Modifiers* modifiers();

  virtual Offset caseAwareTokenOffset();
  virtual Tree* topLevelTmpDef();
  virtual Tree* packageOrPackageObject(Offset start);

  //---------------------------------------------------------------------------
  // STATSEQS
  //---------------------------------------------------------------------------
  virtual Tree* makePackaging(Offset start, Tree* pkg,
    std::vector<Tree*> stats);
  virtual std::vector<Tree*> topStatSeq();
  virtual std::function<std::vector<Tree*> (Token)> topStat();

  virtual Tree* compilationUnit();
  virtual std::vector<Tree*> topstats();

public:
  Global* global;
  Parser(Global* global);
  virtual ~Parser();
  virtual Tree* parse();
};

class SourceFileParser : public Parser {
public:
  SourceFileParser(Global* global);
  virtual ~SourceFileParser();
};

class UnitParser : public SourceFileParser {

private:
  CompilationUnit* unit;

public:
  UnitParser(Global* global, CompilationUnit* unit);
  virtual ~UnitParser();
};

} // namespace

#endif
