#include "c4/scala/NameTransformer.h"

namespace c4s {

/** Constructor */
OpCodes::OpCodes(Char op, std::string code, spOpCodes next)
  : op(op), code(code), next(next) {}

/** Constructor */
NameTransformer::NameTransformer(): op2code(nops), code2op(ncodes) {
  enterOp('~', "$tilde");
  enterOp('=', "$eq");
  enterOp('<', "$less");
  enterOp('>', "$greater");
  enterOp('!', "$bang");
  enterOp('#', "$hash");
  enterOp('%', "$percent");
  enterOp('^', "$up");
  enterOp('&', "$amp");
  enterOp('|', "$bar");
  enterOp('*', "$times");
  enterOp('/', "$div");
  enterOp('+', "$plus");
  enterOp('-', "$minus");
  enterOp(':', "$colon");
  enterOp('\\', "$bslash");
  enterOp('?', "$qmark");
  enterOp('@', "$at");
}

void NameTransformer::enterOp(Char op, std::string code) {
  op2code[op] = code;
  int c = (code[1] - 'a') * 26 + code[2] - 'a';
  code2op[c] = spOpCodes(new OpCodes(op, code, code2op[c]));
}

/** Replace operator symbols by corresponding `\$opname`. */
std::string NameTransformer::encode(std::vector<Char> name) {
  std::vector<Char> buf;
  for (auto c : name) {
    if (c < nops && !(op2code[c].empty())) {
      // TODO:
    }
  }
}

} // namespace
