#include "Token.h"
#include "SymbolTable.h"

namespace djp {

void ST::addSym(int type, int token, int pos, int line,
  const std::string name) {

  spSymbol sym = spSymbol(new Symbol(
    type, scopes.back(), token, pos, line, name));
  symbols.push_back(sym);

  if (isNewScope(type)) {
    scopePush(symbols.size() - 1);
  }
}

/// We match the identifier name with our current scope name.
bool ST::isConstructor(const std::string identifier) {
  spSymbol symScope = scopePeek();
  if (symScope->type == ST_CLASS && symScope->name == identifier) {
    return true;
  }

  return false;
}

bool ST::isNewScope(int type) {
  if (ST_CLASS == type || ST_METHOD == type) {
    return true;
  }

  // TODO: interface, method, inner class and inner interface.

  return false;
}

spSymbol ST::scopePeek() {
  return symbols[scopes.back()];
}

void ST::scopePop(unsigned type) {
  if (type == scopes.back()) {
    scopes.pop_back();
    return;
  }

  // TODO: scope stack is messed up.
}

void ST::scopePush(std::size_t idx) {
  scopes.push_back(idx);
}

} // namespace
