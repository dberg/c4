#include "c4/common/Util.h"

namespace c4 {

u32string itou32s(int i) {
  return itou32s((long) i);
}

u32string itou32s(long i) {
  if (i == 0) return U"0";

  u32string result;
  bool negative = i < 0;// ? 1 : 0;
  i = abs(i);

  while (i > 0) {
    char32_t c = (i % 10) + U'0';
    result += c;
    i /= 10;
  }

  if (negative) {
    result += U'-';
  }

  reverse(result.begin(), result.end());
  return result;
}

u32string itou32s(unsigned int i) {
  return itou32s((unsigned long) i);
}

u32string itou32s(unsigned long i) {
  if (i == 0) return U"0";

  u32string result;

  while (i > 0) {
    char32_t c = (i % 10) + U'0';
    result += c;
    i /= 10;
  }

  reverse(result.begin(), result.end());
  return result;
}

const string itos(unsigned long l) {
  stringstream s; s << l;
  return s.str();
}

const string itos_hex(unsigned long l) {
  stringstream s;
  s << std::hex << l;
  return s.str();
}

} // namespace
