//-*- C++ -*-
#ifndef __TOKEN_H__
#define __TOKEN_H__
#include <map>
#include <string>

namespace djp {

enum Token {
  TOK_EOF = -1,
  TOK_ERROR = -2,

  TOK_ANNOTATION = -3,
  TOK_ANNOTATION_TYPE_DECLARATION = -4,
  TOK_IDENTIFIER = -5,
  TOK_PERIOD = -6,
  TOK_SEMICOLON = -7,

  // Keywords
  TOK_KEY_ABSTRACT = -8,
  TOK_KEY_ASSERT = -9,
  TOK_KEY_BOOLEAN = -10,
  TOK_KEY_BREAK = -11,
  TOK_KEY_BYTE = -12,
  TOK_KEY_CASE = -13,
  TOK_KEY_CATCH = -14,
  TOK_KEY_CHAR = -15,
  TOK_KEY_CLASS = -16,
  TOK_KEY_CONST = -17,
  TOK_KEY_CONTINUE = -18,
  TOK_KEY_DEFAULT = -19,
  TOK_KEY_DO = -20,
  TOK_KEY_DOUBLE = -21,
  TOK_KEY_ELSE = -22,
  TOK_KEY_ENUM = -23,
  TOK_KEY_EXTENDS = -24,
  TOK_KEY_FINAL = -25,
  TOK_KEY_FINALLY = -26,
  TOK_KEY_FLOAT = -27,
  TOK_KEY_FOR = -28,
  TOK_KEY_GOTO = -29,
  TOK_KEY_IF = -30,
  TOK_KEY_IMPLEMENTS = -31,
  TOK_KEY_IMPORT = -32,
  TOK_KEY_INSTANCEOF = -33,
  TOK_KEY_INT = -34,
  TOK_KEY_INTERFACE = -35,
  TOK_KEY_LONG = -36,
  TOK_KEY_NATIVE = -37,
  TOK_KEY_NEW = -38,
  TOK_KEY_PACKAGE = -39,
  TOK_KEY_PRIVATE = -40,
  TOK_KEY_PROTECTED = -41,
  TOK_KEY_PUBLIC = -42,
  TOK_KEY_RETURN = -43,
  TOK_KEY_SHORT = -44,
  TOK_KEY_STATIC = -45,
  TOK_KEY_STRICTFP = -46,
  TOK_KEY_SUPER = -47,
  TOK_KEY_SWITCH = -48,
  TOK_KEY_SYNCHRONIZED = -49,
  TOK_KEY_THIS = -50,
  TOK_KEY_THROW = -51,
  TOK_KEY_THROWS = -52,
  TOK_KEY_TRANSIENT = -53,
  TOK_KEY_TRY = -54,
  TOK_KEY_VOID = -55,
  TOK_KEY_VOLATILE = -56,
  TOK_KEY_WHILE = -57,
};

class TokenUtil {
  typedef std::map<std::string, int> KeywordMap;
  KeywordMap keywords;
public:
  TokenUtil() {
    keywords["abstract"] = TOK_KEY_ABSTRACT;
    keywords["assert"] = TOK_KEY_ASSERT;
    keywords["boolean"] = TOK_KEY_BOOLEAN;
    keywords["break"] = TOK_KEY_BREAK;
    keywords["byte"] = TOK_KEY_BYTE;
    keywords["case"] = TOK_KEY_CASE;
    keywords["catch"] = TOK_KEY_CATCH;
    keywords["char"] = TOK_KEY_CHAR;
    keywords["class"] = TOK_KEY_CLASS;
    keywords["const"] = TOK_KEY_CONST;
    keywords["continue"] = TOK_KEY_CONTINUE;
    keywords["default"] = TOK_KEY_DEFAULT;
    keywords["do"] = TOK_KEY_DO;
    keywords["double"] = TOK_KEY_DOUBLE;
    keywords["else"] = TOK_KEY_ELSE;
    keywords["enum"] = TOK_KEY_ENUM;
    keywords["extends"] = TOK_KEY_EXTENDS;
    keywords["final"] = TOK_KEY_FINAL;
    keywords["finally"] = TOK_KEY_FINALLY;
    keywords["float"] = TOK_KEY_FLOAT;
    keywords["for"] = TOK_KEY_FOR;
    keywords["goto"] = TOK_KEY_GOTO;
    keywords["if"] = TOK_KEY_IF;
    keywords["implements"] = TOK_KEY_IMPLEMENTS;
    keywords["import"] = TOK_KEY_IMPORT;
    keywords["instanceof"] = TOK_KEY_INSTANCEOF;
    keywords["int"] = TOK_KEY_INT;
    keywords["interface"] = TOK_KEY_INTERFACE;
    keywords["long"] = TOK_KEY_LONG;
    keywords["native"] = TOK_KEY_NATIVE;
    keywords["new"] = TOK_KEY_NEW;
    keywords["package"] = TOK_KEY_PACKAGE;
    keywords["private"] = TOK_KEY_PRIVATE;
    keywords["protected"] = TOK_KEY_PROTECTED;
    keywords["public"] = TOK_KEY_PUBLIC;
    keywords["return"] = TOK_KEY_RETURN;
    keywords["short"] = TOK_KEY_SHORT;
    keywords["static"] = TOK_KEY_STATIC;
    keywords["strictfp"] = TOK_KEY_STRICTFP;
    keywords["super"] = TOK_KEY_SUPER;
    keywords["switch"] = TOK_KEY_SWITCH;
    keywords["synchronized"] = TOK_KEY_SYNCHRONIZED;
    keywords["this"] = TOK_KEY_THIS;
    keywords["throw"] = TOK_KEY_THROW;
    keywords["throws"] = TOK_KEY_THROWS;
    keywords["transient"] = TOK_KEY_TRANSIENT;
    keywords["try"] = TOK_KEY_TRY;
    keywords["void"] = TOK_KEY_VOID;
    keywords["volatile"] = TOK_KEY_VOLATILE;
    keywords["while"] = TOK_KEY_WHILE;
  }

  int getKeywordToken(std::string identifier) {
    KeywordMap::iterator it = keywords.find(identifier);
    if (it == keywords.end()) {
      return 0;
    }
    return it->second;
  }
};
} // namespace
#endif
