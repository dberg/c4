#include "utf8/utf8.h"
#include "c4/scala/NameTransformer.h"

namespace c4s {

/** Constructor */
OpCodes::OpCodes(Char op, string code, OpCodes *next)
  : op(op), code(code), next(next) {}

/** Destructor */
OpCodes::~OpCodes() {}

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

/** Destructor */
NameTransformer::~NameTransformer() {
  for (OpCodes* oc : code2op) {
    delete oc;
  }
}

void NameTransformer::enterOp(Char op, string code) {
  op2code[op] = code;
  int c = (code[1] - 'a') * 26 + code[2] - 'a';
  code2op[c] = new OpCodes(op, code, code2op[c]);
}

/** Replace operator symbols by corresponding `\$opname`. */
vector<Char> NameTransformer::encode(vector<Char> name) {
  vector<Char> buf(name.size());
  for (auto c : name) {
    if (c < nops) {
      string code = op2code[c];
      if (code.empty()) {
        buf.push_back(c);
      } else {
        utf8::utf8to16(code.begin(), code.end(), back_inserter(buf));
      }
    // TODO:
    //} else if (!Character.isJavaIdentifierPart(c)) {
      // TODO:
      // buf.append("$u%04X".format(c.toInt))
    } else {
      buf.push_back(c);
    }
  }

  return buf;
}

} // namespace
