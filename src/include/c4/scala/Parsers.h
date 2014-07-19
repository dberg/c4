//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include "c4/scala/TypeDefs.h"
#include "c4/scala/CompilationUnits.h"
#include "c4/scala/Scanners.h"
#include "c4/scala/Trees.h"

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

public:
  Global *global;
  Parser(Global *global): global(global) {}
  virtual spTree parse();
  virtual PackageDef* compilationUnit();
};

class SourceFileParser : public Parser {
public:
  SourceFileParser(Global *global): Parser(global) {}
};

class UnitParser : public SourceFileParser {

private:
  spCompilationUnit unit;

public:
  UnitParser(Global *global, spCompilationUnit unit)
    : SourceFileParser(global), unit(unit) {
    in = spScanner(new UnitScanner(global, unit));
    in->init();
  }
};

} // namespace

#endif
