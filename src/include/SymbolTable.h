//-*- C++ -*-
#ifndef __SYMBOL_TABLE_H__
#define __SYMBOL_TABLE_H__
#include <boost/smart_ptr.hpp>
#include <string>
#include <vector>

namespace djp {

enum SymbolType {
  ST_COMPILATION_UNIT,
  ST_CLASS,
  ST_INTERFACE,
  ST_METHOD,
  ST_TYPE,
};

struct Symbol {
  int type, scope, token, pos, line;
  const std::string name;
  Symbol(int type, int scope, int token, int pos, int line,
    const std::string name) : type(type), scope(scope), token(token),
    pos(pos), line(line), name(name) {}
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

  void addSym(int type, int token, int pos, int line, const std::string name);
  bool isConstructor(const std::string identifier);
  spSymbol scopePeek();
  void scopePop();
  void scopePush(std::size_t idx);

};
} // namespace

#endif
