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

  virtual std::vector<spTree> topstatus();

public:
  Global *global;
  Parser(Global *global);
  virtual spTree parse();
  virtual PackageDef* compilationUnit();
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
