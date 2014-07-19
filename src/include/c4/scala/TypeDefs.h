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

// Names.h
class Names;
typedef std::shared_ptr<Names> spNames;

class TermName;
typedef std::shared_ptr<TermName> spTermName;

// Parsers.h
class UnitParser;
typedef std::shared_ptr<UnitParser> spUnitParser;

// Phase.h
class Phase;
typedef std::shared_ptr<Phase> spPhase;

// Scanners.h
typedef int Offset;

class ScannerData;
typedef std::shared_ptr<ScannerData> spScannerData;

class TokenData;
typedef std::shared_ptr<TokenData> spTokenData;

class Scanner;
typedef std::shared_ptr<Scanner> spScanner;

class UnitScanner;
typedef std::shared_ptr<UnitScanner> spUnitScanner;

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

// Trees.h
class Tree;
typedef std::shared_ptr<Tree> spTree;

class NameTree;
typedef std::shared_ptr<NameTree> spNameTree;

class SymTree;
typedef std::shared_ptr<SymTree> spSymTree;

class DefTree;
typedef std::shared_ptr<DefTree> spDefTree;

class MemberDef;
typedef std::shared_ptr<MemberDef> spMemberDef;

class PackageDef;
typedef std::shared_ptr<PackageDef> spPackageDef;

} // namespace

#endif
