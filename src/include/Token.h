//-*- C++ -*-
#ifndef __TOKEN_H__
#define __TOKEN_H__
#include <map>
#include <string>

namespace djp {

enum Token {
  TOK_EOF = -1,
  TOK_ERROR = -2,

  // Separators
  TOK_LPAREN = -3,
  TOK_RPAREN = -4,
  TOK_LCURLY_BRACKET = -5,
  TOK_RCURLY_BRACKET = -6,
  TOK_LBRACKET = -7,
  TOK_RBRACKET = -8,
  TOK_SEMICOLON = -9,
  TOK_COMMA = -10,
  TOK_PERIOD = -11,

  // Operators
  TOK_OP_EQUALS = -12,
  TOK_OP_GT = -13,
  TOK_OP_LT = -14,
  TOK_OP_EXCLAMATION = -15,
  TOK_OP_TILDE = -16,
  TOK_OP_QUESTION_MARK = -17,
  TOK_OP_COLON = -18,
  TOK_OP_EQUALS_EQUALS = -19,
  TOK_OP_LT_EQUALS = -20,
  TOK_OP_GT_EQUALS = -21,
  TOK_OP_EXCLAMATION_EQUALS = -22,
  TOK_OP_AMPERSAND_AMPERSAND = -23,
  TOK_OP_PIPE_PIPE = -24,
  TOK_OP_PLUS_PLUS = -25,
  TOK_OP_MINUS_MINUS = -26,
  TOK_OP_PLUS = -27,
  TOK_OP_MINUS = -28,
  TOK_MUL = -29,
  TOK_OP_DIV = -30,
  TOK_OP_AMPERSAND = -31,
  TOK_OP_PIPE = -32,
  TOK_OP_CARRET = -33,
  TOK_OP_REM = -34,
  TOK_OP_LSHIFT = -35,
  TOK_OP_RSHIFT = -36,
  TOK_OP_TRIPLE_RSHIFT = -37,
  TOK_OP_PLUS_EQUALS = -38,
  TOK_OP_MINUS_EQUALS = -39,
  TOK_OP_MUL_EQUALS = -40,
  TOK_OP_DIV_EQUALS = -41,
  TOK_OP_AMPERSAND_EQUALS = -42,
  TOK_OP_PIPE_EQUALS = -43,
  TOK_OP_CARRET_EQUALS = -44,
  TOK_OP_REM_EQUALS = -45,
  TOK_OP_LSHIFT_EQUALS = -46,
  TOK_OP_RSHIFT_EQUALS = -47,
  TOK_OP_TRIPLE_RSHIFT_EQUALS = -48,

  // Special symbols
  TOK_ANNOTATION = -49,
  TOK_ANNOTATION_TYPE_DECLARATION = -50,
  TOK_IDENTIFIER = -51,
  TOK_ELLIPSIS = -52,

  TOK_INTEGER_TYPE_SUFFIX = -53, // 'l' or 'L'
  TOK_BOOLEAN_LITERAL = -54,
  TOK_NULL_LITERAL = -55,

  // Numbers
  TOK_DECIMAL_NUMERAL = -56,
  TOK_DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX = -57,
  TOK_HEX_NUMERAL = -58,
  TOK_HEX_NUMERAL_WITH_INT_TYPE_SUFFIX = -59,
  TOK_OCTAL_NUMERAL = -60,
  TOK_OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX = -61,
  TOK_BINARY_NUMERAL = -62,
  TOK_BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX = -63,

  // Keywords
  TOK_KEY_ABSTRACT = -64,
  TOK_KEY_ASSERT = -65,
  TOK_KEY_BOOLEAN = -66,
  TOK_KEY_BREAK = -67,
  TOK_KEY_BYTE = -68,
  TOK_KEY_CASE = -69,
  TOK_KEY_CATCH = -70,
  TOK_KEY_CHAR = -71,
  TOK_KEY_CLASS = -72,
  TOK_KEY_CONST = -73,
  TOK_KEY_CONTINUE = -74,
  TOK_KEY_DEFAULT = -75,
  TOK_KEY_DO = -76,
  TOK_KEY_DOUBLE = -77,
  TOK_KEY_ELSE = -78,
  TOK_KEY_ENUM = -79,
  TOK_KEY_EXTENDS = -80,
  TOK_KEY_FINAL = -81,
  TOK_KEY_FINALLY = -82,
  TOK_KEY_FLOAT = -83,
  TOK_KEY_FOR = -84,
  TOK_KEY_GOTO = -85,
  TOK_KEY_IF = -86,
  TOK_KEY_IMPLEMENTS = -87,
  TOK_KEY_IMPORT = -88,
  TOK_KEY_INSTANCEOF = -89,
  TOK_KEY_INT = -90,
  TOK_KEY_INTERFACE = -91,
  TOK_KEY_LONG = -92,
  TOK_KEY_NATIVE = -93,
  TOK_KEY_NEW = -94,
  TOK_KEY_PACKAGE = -95,
  TOK_KEY_PRIVATE = -96,
  TOK_KEY_PROTECTED = -97,
  TOK_KEY_PUBLIC = -98,
  TOK_KEY_RETURN = -99,
  TOK_KEY_SHORT = -100,
  TOK_KEY_STATIC = -101,
  TOK_KEY_STRICTFP = -102,
  TOK_KEY_SUPER = -103,
  TOK_KEY_SWITCH = -104,
  TOK_KEY_SYNCHRONIZED = -105,
  TOK_KEY_THIS = -106,
  TOK_KEY_THROW = -107,
  TOK_KEY_THROWS = -108,
  TOK_KEY_TRANSIENT = -109,
  TOK_KEY_TRY = -110,
  TOK_KEY_VOID = -111,
  TOK_KEY_VOLATILE = -112,
  TOK_KEY_WHILE = -113,
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
