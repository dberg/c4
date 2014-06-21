//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>

namespace c4s {

typedef unsigned short Char;

class Global;
typedef std::shared_ptr<Global> spGlobal;

class SubComponent;
typedef std::shared_ptr<SubComponent> spSubComponent;

class SyntaxAnalyzer;
typedef std::shared_ptr<SyntaxAnalyzer> spSyntaxAnalyzer;

// ----------------------------------------------------------------------------
// Phase
// ----------------------------------------------------------------------------
class Phase;
typedef std::shared_ptr<Phase> spPhase;

class NoPhase;
typedef std::shared_ptr<NoPhase> spNoPhase;

class GlobalPhase;
typedef std::shared_ptr<GlobalPhase> spGlobalPhase;

class StdPhase;
typedef std::shared_ptr<StdPhase> spStdPhase;

class ParserPhase;
typedef std::shared_ptr<ParserPhase> spParserPhase;

} // namespace

#endif
