#include "Token.h"
#include "SymbolTable.h"

namespace djp {

void ST::addSym(int type, unsigned pos, unsigned end, unsigned line,
  const std::string metadata) {

  spSymbol sym = spSymbol(new Symbol(
    type, scopes.back(), pos, end, line, metadata));
  symbols.push_back(sym);

  if (isNewScope(type)) {
    scopePush(symbols.size() - 1);
  }
}

/// We match the identifier name with our current scope name.
bool ST::isConstructor(const std::string identifier) {
  // If the current scope is of type ST_MEMBER_DECL the previous scope should be
  // of type ST_CLASS. Given that the first scope is a compilation unit the
  // scope size should be equal or greater than 3 at this point.
  if (scopes.size() < 3) { return false; }
  std::vector<std::size_t>::size_type classId = scopes[scopes.size() - 2];
  spSymbol symClass = symbols[classId];
  if (symClass->type != ST_CLASS) {
    return false;
  }

  // The next symbol after ST_CLASS should be of type ST_IDENTIFIER.
  spSymbol symId = symbols[classId + 1];
  if (symId->type == ST_IDENTIFIER && symId->metadata == identifier) {
    return true;
  }

  return false;
}

bool ST::isNewScope(int type) {
  if (type == ST_CLASS_OR_INTERFACE
    || type == ST_MEMBER_DECL
    || type == ST_CLASS
    || type == ST_ENUM
    || type == ST_INTERFACE
    || type == ST_METHOD) {
    return true;
  }

  return false;
}

void ST::scopePop() {
  assert (scopes.size() > 0);
  scopes.pop_back();
}

void ST::scopePush(std::size_t idx) {
  scopes.push_back(idx);
}

void ST::updateScopeType(int type) {
  symbols[scopes.back()]->type = type;
}

void ST::updateScopeEnd(unsigned end) {
  symbols[scopes.back()]->end = end;
}

} // namespace
