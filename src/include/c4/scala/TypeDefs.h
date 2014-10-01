//-*- C++ -*-
#ifndef __C4_SCALA_TYPE_DEFS_H__
#define __C4_SCALA_TYPE_DEFS_H__

#include <memory>
#include <string>
#include "c4/common/TypeDefs.h"

namespace c4s {

typedef c4::Char Char;

// Global.h
class Global;

// Names.h
class Names;
class Name;
class TermName;
class TypeName;
typedef Name ThisNameType;

// NameTransformer.h
class NameTransformer;
class OpCodes;

// Parsers.h
class UnitParser;

// Phase.h
class Phase;

// Position.h
class Position;

// Positions.h
class PosAssigner;
class Positions;

// Printers.h
class Printers;

// Scanners.h
typedef int Offset;
class ScannerData;
class TokenData;
class Scanner;
class UnitScanner;

// SourceFile.h
class SourceFile;
class ClientSourceFile;

// StdNames.h
typedef std::string NameType;
class StdNames;
class Keywords;
class KeywordSetBuilder;

// SubComponent.h
class SubComponent;

// Symbols.h
class Symbol;

// SyntaxAnalyzer.h
class SyntaxAnalyzer;

// Trees.h
class Tree;
class Ident;

class Modifiers;
typedef std::shared_ptr<Modifiers> spModifiers;

} // namespace

#endif
