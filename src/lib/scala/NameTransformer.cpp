#include "c4/scala/NameTransformer.h"

namespace c4s {

/** Constructor */
OpCodes::OpCodes(char32_t op, u32string code, OpCodes* next)
  : op(op), code(code), next(next) {}

/** Destructor */
OpCodes::~OpCodes() {}

/** Constructor */
NameTransformer::NameTransformer(): op2code(nops), code2op(ncodes) {
  // TODO:
  //enterOp('~', "$tilde");
  //enterOp('=', "$eq");
  //enterOp('<', "$less");
  //enterOp('>', "$greater");
  //enterOp('!', "$bang");
  //enterOp('#', "$hash");
  //enterOp('%', "$percent");
  //enterOp('^', "$up");
  //enterOp('&', "$amp");
  //enterOp('|', "$bar");
  //enterOp('*', "$times");
  //enterOp('/', "$div");
  //enterOp('+', "$plus");
  //enterOp('-', "$minus");
  //enterOp(':', "$colon");
  //enterOp('\\', "$bslash");
  //enterOp('?', "$qmark");
  //enterOp('@', "$at");
}

/** Destructor */
NameTransformer::~NameTransformer() {
  for (OpCodes* oc : code2op) {
    delete oc;
  }
}

void NameTransformer::enterOp(char32_t op, u32string code) {
  // TODO:
  //op2code[op] = code;
  //int c = (code[1] - 'a') * 26 + code[2] - 'a';
  //code2op[c] = new OpCodes(op, code, code2op[c]);
}

/** Replace operator symbols by corresponding `\$opname`. */
u32string NameTransformer::encode(u32string name) {
  // TODO:
  u32string buf;
  return buf;
}

} // namespace
