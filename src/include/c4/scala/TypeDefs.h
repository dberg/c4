//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>

namespace c4s {

// CharArrayReader.h
class CharArrayReader;
typedef std::shared_ptr<CharArrayReader> spCharArrayReader;

// Compilation Unit.h
class CompilationUnit;
typedef std::shared_ptr<CompilationUnit> spCompilationUnit;

// Global.h
class Global;
typedef std::shared_ptr<Global> spGlobal;

// Phase.h
class Phase;
typedef std::shared_ptr<Phase> spPhase;

// Scanners.h
class ScannerData;
typedef std::shared_ptr<ScannerData> spScannerData;

// SourceFile.h
class SourceFile;
typedef std::shared_ptr<SourceFile> spSourceFile;

class ClientSourceFile;
typedef std::shared_ptr<ClientSourceFile> spClientSourceFile;

// SubComponent.h
class SubComponent;

// SyntaxAnalyzer.h
class SyntaxAnalyzer;
typedef std::shared_ptr<SyntaxAnalyzer> spSyntaxAnalyzer;

} // namespace

#endif
