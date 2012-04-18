//-*- C++ -*-
#ifndef __TOKEN_H__
#define __TOKEN_H__

namespace djp {
enum Token {
  TOK_EOF = -1,
  TOK_ERROR = -2,
  TOK_ANNOTATION = -3,
  TOK_ANNOTATION_TYPE_DECLARATION = -4,
  TOK_IDENTIFIER = -5,
  TOK_PACKAGE = -6,
};
}

#endif
