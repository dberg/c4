//-*- C++ -*-
#ifndef __C4_JAVA_LITERAL_SUPPORT_H__
#define __C4_JAVA_LITERAL_SUPPORT_H__

#include <memory>
#include <string>

#include "c4/java/Diagnosis.h"
#include "c4/java/SourceCodeStream.h"

using std::shared_ptr;
using std::u32string;

namespace c4j {

enum class LiteralToken {
  ERROR,
  BINARY_NUMERAL,
  BINARY_NUMERAL_WITH_INT_TYPE_SUFFIX,
  DECIMAL_NUMERAL,
  DECIMAL_NUMERAL_WITH_INT_TYPE_SUFFIX,
  DECIMAL_FLOATING_POINT,
  HEX_NUMERAL,
  HEX_NUMERAL_WITH_INT_TYPE_SUFFIX,
  HEXADECIMAL_FLOATING_POINT_LITERAL,
  OCTAL_NUMERAL,
  OCTAL_NUMERAL_WITH_INT_TYPE_SUFFIX,
};

// Helpers
bool isBinaryDigit(char c);
bool isDecimalDigit(char c);
bool isHexDigit(char c);
bool isOctalDigit(char c);

class LiteralSupport;
typedef shared_ptr<LiteralSupport> spLiteralSupport;

class LiteralSupport {

  spDiagnosis diag;
  spSourceCodeStream src;

  int consumeDigitsPOrUnderscores(u32string &ss, bool (*fnDigitP) (char));
  int consumeExponentPart(u32string &ss);

  LiteralToken getBinaryNumeral(u32string &ss);
  LiteralToken getDecimalNumeral(u32string &ss);
  LiteralToken getHexNumeral(u32string &ss);
  LiteralToken getOctalNumeral(u32string &ss);
  LiteralToken getTokWithLeadingZero(u32string &ss);

public:
  LiteralSupport(spSourceCodeStream &src, spDiagnosis &diag)
    : diag(diag), src(src) {}
  LiteralToken getLiteralNumber(char c, u32string &ss);
  LiteralToken getDecimalNumeralOrDecimalFloatingPoint(
    char previous_c, u32string &ss);
  LiteralToken getDecimalFloatingPointStartingWithAPeriod(u32string &ss);
};

} // namespace

#endif
