//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>
#include "c4/common/TypeDefs.h"

namespace c4s {

typedef c4::Char Char;

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

class Name;
typedef std::shared_ptr<Name> spName;

class TermName;
typedef std::shared_ptr<TermName> spTermName;

class TypeName;
typedef std::shared_ptr<TypeName> spTypeName;

typedef Name ThisNameType;
typedef spName spThisNameType;

// NameTransformer.h
class NameTransformer;
typedef std::shared_ptr<NameTransformer> spNameTransformer;

class OpCodes;
typedef std::shared_ptr<OpCodes> spOpCodes;

// Parsers.h
class UnitParser;
typedef std::shared_ptr<UnitParser> spUnitParser;

// Phase.h
class Phase;
typedef std::shared_ptr<Phase> spPhase;

// Positions.h
class Positions;
typedef std::shared_ptr<Positions> spPositions;

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

// StdNames.h
class StdNames;
typedef std::shared_ptr<StdNames> spStdNames;

class Keywords;
typedef std::shared_ptr<Keywords> spKeywords;

class KeywordSetBuilder;
typedef std::shared_ptr<KeywordSetBuilder> spKeywordSetBuilder;

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

class Ident;

} // namespace

#endif
