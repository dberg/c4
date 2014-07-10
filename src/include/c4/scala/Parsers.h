//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include "c4/scala/CompilationUnits.h"
#include "c4/scala/ParsersTypeDefs.h"
#include "c4/scala/ScannersTypeDefs.h"
#include "c4/scala/Scanners.h"
#include "c4/scala/Trees.h"

namespace c4s {

class ParserCommon {

};

class Parser : public ParserCommon {
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
  virtual spTree parse();
  virtual PackageDef* compilationUnit();
};

class SourceFileParser : public Parser {

};

class UnitParser : public SourceFileParser {

private:
  spCompilationUnit unit;

public:
  UnitParser(spCompilationUnit unit) : unit(unit) {
    // TODO:
    //in = spUnitScanner(new UnitScanner(unit));
    // TODO:
    //in.init();
  }
};

} // namespace

#endif
