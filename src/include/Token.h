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
  TOK_ELLIPSIS = -7,
  TOK_COMMA = -8,
  TOK_SEMICOLON = -9,
  TOK_ASTERISK = -10,
  TOK_LCURLY_BRACKET = -11,
  TOK_RCURLY_BRACKET = -12,
  TOK_LBRACKET = -13,
  TOK_RBRACKET = -14,
  TOK_LPAREN = -15,
  TOK_RPAREN = -16,
  TOK_OP_PLUS_PLUS = -17,
  TOK_OP_MINUS_MINUS = -18,
  TOK_OP_EXCLAMATION = -19,
  TOK_OP_TILDE = -20,
  TOK_OP_PLUS = -21,
  TOK_OP_MINUS = -22,
  TOK_EQUALS = -23,
  TOK_EQUALS_EQUALS = -24,

  // Numbers
  TOK_DECIMAL_NUMERAL = -25,

  // Keywords
  TOK_KEY_ABSTRACT = -26,
  TOK_KEY_ASSERT = -27,
  TOK_KEY_BOOLEAN = -28,
  TOK_KEY_BREAK = -29,
  TOK_KEY_BYTE = -30,
  TOK_KEY_CASE = -31,
  TOK_KEY_CATCH = -32,
  TOK_KEY_CHAR = -33,
  TOK_KEY_CLASS = -34,
  TOK_KEY_CONST = -35,
  TOK_KEY_CONTINUE = -36,
  TOK_KEY_DEFAULT = -37,
  TOK_KEY_DO = -38,
  TOK_KEY_DOUBLE = -39,
  TOK_KEY_ELSE = -40,
  TOK_KEY_ENUM = -41,
  TOK_KEY_EXTENDS = -42,
  TOK_KEY_FINAL = -43,
  TOK_KEY_FINALLY = -44,
  TOK_KEY_FLOAT = -45,
  TOK_KEY_FOR = -46,
  TOK_KEY_GOTO = -47,
  TOK_KEY_IF = -48,
  TOK_KEY_IMPLEMENTS = -49,
  TOK_KEY_IMPORT = -50,
  TOK_KEY_INSTANCEOF = -51,
  TOK_KEY_INT = -52,
  TOK_KEY_INTERFACE = -53,
  TOK_KEY_LONG = -54,
  TOK_KEY_NATIVE = -55,
  TOK_KEY_NEW = -56,
  TOK_KEY_PACKAGE = -57,
  TOK_KEY_PRIVATE = -58,
  TOK_KEY_PROTECTED = -59,
  TOK_KEY_PUBLIC = -60,
  TOK_KEY_RETURN = -61,
  TOK_KEY_SHORT = -62,
  TOK_KEY_STATIC = -63,
  TOK_KEY_STRICTFP = -64,
  TOK_KEY_SUPER = -65,
  TOK_KEY_SWITCH = -66,
  TOK_KEY_SYNCHRONIZED = -67,
  TOK_KEY_THIS = -68,
  TOK_KEY_THROW = -69,
  TOK_KEY_THROWS = -70,
  TOK_KEY_TRANSIENT = -71,
  TOK_KEY_TRY = -72,
  TOK_KEY_VOID = -73,
  TOK_KEY_VOLATILE = -74,
  TOK_KEY_WHILE = -75,
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

    tokensLen[TOK_KEY_ABSTRACT] = 8;
    tokensLen[TOK_KEY_ASSERT] = 6;
    tokensLen[TOK_KEY_BOOLEAN] = 7;
    tokensLen[TOK_KEY_BREAK] = 5;
    tokensLen[TOK_KEY_BYTE] = 4;
    tokensLen[TOK_KEY_CASE] = 4;
    tokensLen[TOK_KEY_CATCH] = 5;
    tokensLen[TOK_KEY_CHAR] = 4;
    tokensLen[TOK_KEY_CLASS] = 5;
    tokensLen[TOK_KEY_CONST] = 5;
    tokensLen[TOK_KEY_CONTINUE] = 8;
    tokensLen[TOK_KEY_DEFAULT] = 7;
    tokensLen[TOK_KEY_DO] = 2;
    tokensLen[TOK_KEY_DOUBLE] = 6;
    tokensLen[TOK_KEY_ELSE] = 4;
    tokensLen[TOK_KEY_ENUM] = 4;
    tokensLen[TOK_KEY_EXTENDS] = 7;
    tokensLen[TOK_KEY_FINAL] = 5;
    tokensLen[TOK_KEY_FINALLY] = 7;
    tokensLen[TOK_KEY_FLOAT] = 5;
    tokensLen[TOK_KEY_FOR] = 3;
    tokensLen[TOK_KEY_GOTO] = 4;
    tokensLen[TOK_KEY_IF] = 2;
    tokensLen[TOK_KEY_IMPLEMENTS] = 10;
    tokensLen[TOK_KEY_IMPORT] = 6;
    tokensLen[TOK_KEY_INSTANCEOF] = 10;
    tokensLen[TOK_KEY_INT] = 3;
    tokensLen[TOK_KEY_INTERFACE] = 9;
    tokensLen[TOK_KEY_LONG] = 4;
    tokensLen[TOK_KEY_NATIVE] = 6;
    tokensLen[TOK_KEY_NEW] = 3;
    tokensLen[TOK_KEY_PACKAGE] = 7;
    tokensLen[TOK_KEY_PRIVATE] = 7;
    tokensLen[TOK_KEY_PROTECTED] = 9;
    tokensLen[TOK_KEY_PUBLIC] = 6;
    tokensLen[TOK_KEY_RETURN] = 6;
    tokensLen[TOK_KEY_SHORT] = 5;
    tokensLen[TOK_KEY_STATIC] = 6;
    tokensLen[TOK_KEY_STRICTFP] = 8;
    tokensLen[TOK_KEY_SUPER] = 5;
    tokensLen[TOK_KEY_SWITCH] = 6;
    tokensLen[TOK_KEY_SYNCHRONIZED] = 12;
    tokensLen[TOK_KEY_THIS] = 4;
    tokensLen[TOK_KEY_THROW] = 5;
    tokensLen[TOK_KEY_THROWS] = 6;
    tokensLen[TOK_KEY_TRANSIENT] = 9;
    tokensLen[TOK_KEY_TRY] = 3;
    tokensLen[TOK_KEY_VOID] = 4;
    tokensLen[TOK_KEY_VOLATILE] = 8;
    tokensLen[TOK_KEY_WHILE] = 5;
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
