//-*- C++ -*-
#ifndef __SYMBOL_TABLE_H__
#define __SYMBOL_TABLE_H__
#include <boost/smart_ptr.hpp>
#include <string>
#include <vector>

namespace djp {

enum SymbolType {
  // Logical units
  ST_COMPILATION_UNIT,
  // overwritten by ST_CLASS, ST_INTERFACE or ST_ENUM
  ST_CLASS_OR_INTERFACE,
  // overwritten by ST_METHOD or ST_FIELD
  ST_MEMBER_DECL,
  ST_CLASS,
  ST_ENUM,
  ST_INTERFACE,
  ST_METHOD,
  ST_FIELD,
  ST_INNER_CLASS,
  ST_INNER_INTERFACE,

  // Symbols
  ST_IDENTIFIER,
  ST_TYPE,
};

struct Symbol {
  int type;
  unsigned scope, pos, end, line;
  const std::string metadata;
  Symbol(int type, unsigned scope, unsigned pos, unsigned end, unsigned line,
    const std::string metadata)
    : type(type), scope(scope), pos(pos), end(end), line(line),
      metadata(metadata) {}
};

typedef boost::shared_ptr<struct Symbol> spSymbol;

class ST {
  bool isNewScope(int type);

public:
  std::vector<std::size_t> scopes;
  std::vector<spSymbol> symbols;

  ST() {
    scopes.push_back(0);
    addSym(ST_COMPILATION_UNIT, 0, 0, 0, "");
  }

  void addSym(int type, unsigned pos, unsigned end, unsigned line,
    const std::string metadata);
  bool isConstructor(const std::string identifier);
  void scopePop();
  void scopePush(std::size_t idx);
  void updateScopeType(int type);
  void updateScopeEnd(unsigned end);

};
} // namespace

#endif
