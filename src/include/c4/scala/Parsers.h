//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#include "c4/scala/CompilationUnits.h"
#include "c4/scala/ParsersTypeDefs.h"
#include "c4/scala/Trees.h"

namespace c4s {

/* Notes:
// CompilationUnit ::= {package QualId semi} TopStatSeq
def compilationUnit(): PackageDef
*/

class ParserCommon {

};

class Parser : public ParserCommon {
public:
  virtual spTree parse();
};

class SourceFileParser : public Parser {

};

class UnitParser : public SourceFileParser {

private:
  spCompilationUnit unit;

public:
  UnitParser(spCompilationUnit unit) : unit(unit) {}
};

} // namespace

#endif
