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
  TOK_ASTERISK = -8,

  // Keywords
  TOK_KEY_ABSTRACT = -9,
  TOK_KEY_ASSERT = -10,
  TOK_KEY_BOOLEAN = -11,
  TOK_KEY_BREAK = -12,
  TOK_KEY_BYTE = -13,
  TOK_KEY_CASE = -14,
  TOK_KEY_CATCH = -15,
  TOK_KEY_CHAR = -16,
  TOK_KEY_CLASS = -17,
  TOK_KEY_CONST = -18,
  TOK_KEY_CONTINUE = -19,
  TOK_KEY_DEFAULT = -20,
  TOK_KEY_DO = -21,
  TOK_KEY_DOUBLE = -22,
  TOK_KEY_ELSE = -23,
  TOK_KEY_ENUM = -24,
  TOK_KEY_EXTENDS = -25,
  TOK_KEY_FINAL = -26,
  TOK_KEY_FINALLY = -27,
  TOK_KEY_FLOAT = -28,
  TOK_KEY_FOR = -29,
  TOK_KEY_GOTO = -30,
  TOK_KEY_IF = -31,
  TOK_KEY_IMPLEMENTS = -32,
  TOK_KEY_IMPORT = -33,
  TOK_KEY_INSTANCEOF = -34,
  TOK_KEY_INT = -35,
  TOK_KEY_INTERFACE = -36,
  TOK_KEY_LONG = -37,
  TOK_KEY_NATIVE = -38,
  TOK_KEY_NEW = -39,
  TOK_KEY_PACKAGE = -40,
  TOK_KEY_PRIVATE = -41,
  TOK_KEY_PROTECTED = -42,
  TOK_KEY_PUBLIC = -43,
  TOK_KEY_RETURN = -44,
  TOK_KEY_SHORT = -45,
  TOK_KEY_STATIC = -46,
  TOK_KEY_STRICTFP = -47,
  TOK_KEY_SUPER = -48,
  TOK_KEY_SWITCH = -49,
  TOK_KEY_SYNCHRONIZED = -50,
  TOK_KEY_THIS = -51,
  TOK_KEY_THROW = -52,
  TOK_KEY_THROWS = -53,
  TOK_KEY_TRANSIENT = -54,
  TOK_KEY_TRY = -55,
  TOK_KEY_VOID = -56,
  TOK_KEY_VOLATILE = -57,
  TOK_KEY_WHILE = -58,
};

class TokenUtil {
  typedef std::map<std::string, int> KeywordMap;
  KeywordMap keywords;

  typedef std::map<int, int> TokenLengthMap;
  TokenLengthMap tokensLen;

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

    tokensLen[TOK_KEY_PACKAGE] = 7;
    tokensLen[TOK_KEY_IMPORT] = 6;
    tokensLen[TOK_KEY_STATIC] = 6;
    tokensLen[TOK_KEY_PUBLIC] = 6;
  }

  int getKeywordToken(std::string identifier) {
    KeywordMap::iterator it = keywords.find(identifier);
    if (it == keywords.end()) {
      return 0;
    }
    return it->second;
  }

  int getTokenLength(int token) {
    TokenLengthMap::iterator it = tokensLen.find(token);
    if (it == tokensLen.end()) {
      return 0;
    }
    return it->second;
  }
};

  enum ImportType {
    SINGLE_TYPE_IMPORT_DECLARATION,
    TYPE_IMPORT_ON_DEMAND_DECLARATION,
    SINGLE_STATIC_IMPORT_DECLARATION,
    STATIC_IMPORT_ON_DEMAND_DECLARATION,
  };

} // namespace
#endif
