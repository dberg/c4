//-*- C++ -*-
#ifndef __C4_JAVA_LITERAL_SUPPORT_H__
#define __C4_JAVA_LITERAL_SUPPORT_H__

#include <memory>
#include <sstream>

#include "c4/java/Diagnosis.h"
#include "c4/java/SourceCodeStream.h"

using std::shared_ptr;
using std::string;
using std::stringstream;

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
typedef shared_ptr<LiteralSupport> spLiteralSupport;

class LiteralSupport {

  spDiagnosis diag;
  spSourceCodeStream src;

  int consumeDigitsPOrUnderscores(stringstream &ss, bool (*fnDigitP) (char));
  int consumeExponentPart(stringstream &ss);

  LiteralToken getBinaryNumeral(stringstream &ss);
  LiteralToken getDecimalNumeral(stringstream &ss);
  LiteralToken getHexNumeral(stringstream &ss);
  LiteralToken getOctalNumeral(stringstream &ss);
  LiteralToken getTokWithLeadingZero(stringstream &ss);

public:
  LiteralSupport(spSourceCodeStream &src, spDiagnosis &diag)
    : diag(diag), src(src) {}
  LiteralToken getLiteralNumber(char c, stringstream &ss);
  LiteralToken getDecimalNumeralOrDecimalFloatingPoint(
    char previous_c, stringstream &ss);
  LiteralToken getDecimalFloatingPointStartingWithAPeriod(stringstream &ss);
};

} // namespace

#endif
