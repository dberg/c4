//-*- C++ -*-
#ifndef __LITERAL_SUPPORT_H__
#define __LITERAL_SUPPORT_H__
#include <memory>
#include <sstream>
#include "djp/SourceCodeStream.h"
#include "Diagnosis.h"
#include "Token.h"

namespace djp {

class LiteralSupport;
typedef std::shared_ptr<LiteralSupport> spLiteralSupport;

class LiteralSupport {

  spDiagnosis diag;
  spSourceCodeStream src;

  int consumeDigitsPOrUnderscores(
    std::stringstream &ss, bool (*fnDigitP) (char));
  int consumeExponentPart(std::stringstream &ss);

  int getBinaryNumeral(std::stringstream &ss);
  int getDecimalNumeral(std::stringstream &ss);
  int getHexNumeral(std::stringstream &ss);
  int getOctalNumeral(std::stringstream &ss);
  int getTokWithLeadingZero(std::stringstream &ss);

public:
  LiteralSupport(spSourceCodeStream &src, spDiagnosis &diag)
    : diag(diag), src(src) {}
  int getLiteralNumber(char c, std::stringstream &ss);
  int getDecimalNumeralOrDecimalFloatingPoint(
    char previous_c, std::stringstream &ss);
  int getDecimalFloatingPointStartingWithAPeriod(std::stringstream &ss);
};

} // namespace

#endif
