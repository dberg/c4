//-*- C++ -*-
#ifndef __C4_SCALA_PARSERS_H__
#define __C4_SCALA_PARSERS_H__

#incude "c4/scala/ParsersTypeDefs.h"

namespace c4s {

/* Notes:

ParserCommon
Parser
SourceFileParser
UnitParser

// CompilationUnit ::= {package QualId semi} TopStatSeq
def compilationUnit(): PackageDef

*/

class Parser : public ParserCommon {

};

class SourceFileParser : public Parser {

};

class UnitParser : public SourceFileParser {

};

} // namespace

#endif
