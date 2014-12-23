//-*- C++ -*-
#ifndef __C4_JAVA_SYMBOL_TABLE_H__
#define __C4_JAVA_SYMBOL_TABLE_H__

#include <cassert>
#include <memory>
#include <string>
#include <vector>

using std::shared_ptr;
using std::string;
using std::vector;

namespace c4j {

enum SymbolType {
  // Logical units
  ST_COMPILATION_UNIT,
  ST_PACKAGE,
  // overwritten by ST_CLASS, ST_INTERFACE or ST_ENUM
  ST_CLASS_OR_INTERFACE,
  // overwritten by ST_METHOD or ST_FIELD
  ST_MEMBER_DECL,
  ST_CLASS,
  ST_ENUM,
  ST_INTERFACE,
  ST_METHOD,
  ST_FIELD,

  // Symbols
  ST_IDENTIFIER,
  ST_TYPE,
};

struct Symbol {
  int type;
  unsigned scope, pos, end, line;
  const string metadata;
  Symbol(int type, unsigned scope, unsigned pos, unsigned end, unsigned line,
    const string metadata)
    : type(type), scope(scope), pos(pos), end(end), line(line),
      metadata(metadata) {}
};

typedef shared_ptr<struct Symbol> spSymbol;

class ST {
  bool isNewScope(int type);

public:
  vector<std::size_t> scopes;
  vector<spSymbol> symbols;

  ST() {
    scopes.push_back(0);
    addSym(ST_COMPILATION_UNIT, 0, 0, 0, "");
  }

  void addSym(int type, unsigned pos, unsigned end, unsigned line,
    const string metadata);
  bool isConstructor(const string identifier);
  void scopePop();
  void scopePush(std::size_t idx);
  void updateScopeType(int type);
  void updateScopeEnd(unsigned end);

};
} // namespace

#endif
