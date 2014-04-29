//-*- C++ -*-
#ifndef __SCALA_PARSER_STATE_H__
#define __SCALA_PARSER_STATE_H__
#include <string>
#include "c4/scala/Token.h"

namespace c4s {

// Current State of the Parser. We use this to perform lookaheads, backtrack
// or consult previous state data if necessary.
struct State {
  unsigned indentationLevel;
  unsigned indentationMapSize;
  unsigned diagErrorsSize;
  unsigned cursor;
  unsigned line;
  STok token;
  std::string tokenStr;

  State() : diagErrorsSize(0), cursor(0), line(0) {}
};

} // namespace
#endif
