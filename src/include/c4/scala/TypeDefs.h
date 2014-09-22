//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>
#include <string>
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

class OpCodes;
typedef std::shared_ptr<OpCodes> spOpCodes;

// Parsers.h
class UnitParser;
typedef std::shared_ptr<UnitParser> spUnitParser;

// Phase.h
class Phase;

// Position.h
class Position;
typedef std::shared_ptr<Position> spPosition;

// Positions.h
class PosAssigner;
typedef std::shared_ptr<PosAssigner> spPosAssigner;

class Positions;

// Printers.h
class Printers;

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
typedef std::string NameType;

class StdNames;

class Keywords;
typedef std::shared_ptr<Keywords> spKeywords;

class KeywordSetBuilder;
typedef std::shared_ptr<KeywordSetBuilder> spKeywordSetBuilder;

// SubComponent.h
class SubComponent;

// Symbols.h
class Symbol;
typedef std::shared_ptr<Symbol> spSymbol;

// SyntaxAnalyzer.h
class SyntaxAnalyzer;

// Trees.h
class Tree;
typedef std::shared_ptr<Tree> spTree;

class Ident;

class Modifiers;
typedef std::shared_ptr<Modifiers> spModifiers;

} // namespace

#endif
