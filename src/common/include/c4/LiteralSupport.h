//-*- C++ -*-
#ifndef __LITERAL_SUPPORT_H__
#define __LITERAL_SUPPORT_H__
#include <memory>
#include <sstream>
#include "c4/SourceCodeStream.h"
#include "c4/Diagnosis.h"

namespace c4 {

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
typedef std::shared_ptr<LiteralSupport> spLiteralSupport;

class LiteralSupport {

  spDiagnosis diag;
  spSourceCodeStream src;

  int consumeDigitsPOrUnderscores(
    std::stringstream &ss, bool (*fnDigitP) (char));
  int consumeExponentPart(std::stringstream &ss);

  LiteralToken getBinaryNumeral(std::stringstream &ss);
  LiteralToken getDecimalNumeral(std::stringstream &ss);
  LiteralToken getHexNumeral(std::stringstream &ss);
  LiteralToken getOctalNumeral(std::stringstream &ss);
  LiteralToken getTokWithLeadingZero(std::stringstream &ss);

public:
  LiteralSupport(spSourceCodeStream &src, spDiagnosis &diag)
    : diag(diag), src(src) {}
  LiteralToken getLiteralNumber(char c, std::stringstream &ss);
  LiteralToken getDecimalNumeralOrDecimalFloatingPoint(
    char previous_c, std::stringstream &ss);
  LiteralToken getDecimalFloatingPointStartingWithAPeriod(
    std::stringstream &ss);
};

} // namespace

#endif
