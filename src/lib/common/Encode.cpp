#include "c4/common/Encode.h"

namespace c4 {

u32string utf8_to_u32(string s) {
  u32string result;
  utf8::utf8to32(s.begin(), s.end(), std::back_inserter(result));
  return result;
}

string u32_to_utf8(u32string s) {
  string result;
  utf8::utf32to8(s.begin(), s.end(), std::back_inserter(result));
  return result;
}

} // namespace
