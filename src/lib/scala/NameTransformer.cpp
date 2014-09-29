#include "utf8/utf8.h"
#include "c4/scala/NameTransformer.h"

namespace c4s {

/** Constructor */
OpCodes::OpCodes(Char op, std::string code, spOpCodes next)
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

void NameTransformer::enterOp(Char op, std::string code) {
  op2code[op] = code;
  int c = (code[1] - 'a') * 26 + code[2] - 'a';
  code2op[c] = spOpCodes(new OpCodes(op, code, code2op[c]));
}

/** Replace operator symbols by corresponding `\$opname`. */
std::vector<Char> NameTransformer::encode(std::vector<Char> name) {
  std::vector<Char> buf(name.size());
  for (auto c : name) {
    if (c < nops) {
      std::string code = op2code[c];
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
