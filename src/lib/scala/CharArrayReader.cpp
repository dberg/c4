#include "c4/scala/CharArrayReader.h"
#include "c4/scala/Chars.h"

namespace c4s {

CharArrayReader::CharArrayReader(std::vector<c4::Char> &buf): buf(buf) {}

/** Advance lone character; reducing CR;LF pairs to just LF */
void CharArrayReader::nextChar() {
  // TODO:
  if (charOffset >= buf.size()) {
    ch = Chars::SU;
  } else {
    c4::Char c = buf[charOffset++];
    ch = c;

    if (c == '\\') {
      // TODO:
      // potentialUnicode()
    }

    if (ch < ' ') {
      // TODO:
      //skipCR();
      //potentialLineEnd();
    }
  }
}

} // namespace
