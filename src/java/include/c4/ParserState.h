//-*- C++ -*-
#ifndef __PARSER_STATE_H__
#define __PARSER_STATE_H__
#include <string>

namespace c4 {

// Current State of the Parser. We use this to perform lookaheads, backtrack
// or consult previous state data if necessary.
struct State {
  unsigned indentationLevel;
  unsigned indentationMapSize;
  unsigned diagErrorsSize;
  unsigned cursor;
  unsigned line;
  int token;
  std::string tokenStr;
};

} // namespace
#endif
