//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>

namespace c4s {

class CompilationUnit;
typedef std::shared_ptr<CompilationUnit> spCompilationUnit;

class Global;
typedef std::shared_ptr<Global> spGlobal;

class SubComponent;

class SyntaxAnalyzer;
typedef std::shared_ptr<SyntaxAnalyzer> spSyntaxAnalyzer;

class Phase;
typedef std::shared_ptr<Phase> spPhase;

//class ParserPhase;

} // namespace

#endif
