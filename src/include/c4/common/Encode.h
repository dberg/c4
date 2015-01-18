//-*- C++ -*-
#ifndef __C4J_ENCODE_H__
#define __C4J_ENCODE_H__

#include <string>
#include "utf8/utf8.h"

using std::string;
using std::u32string;

namespace c4 {

u32string utf8_to_u32(string s);
string u32_to_utf8(u32string s);

}

#endif
