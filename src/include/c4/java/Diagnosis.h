//-*- C++ -*-
#ifndef __C4_JAVA_DIAGNOSIS_H__
#define __C4_JAVA_DIAGNOSIS_H__

#include <vector>
#include "c4/common/ErrorCodes.h"

using std::shared_ptr;
using std::vector;

namespace c4j {

class Diagnosis;
typedef shared_ptr<Diagnosis> spDiagnosis;

class Diagnosis {
public:
  vector<c4::spError> errors;

  int addErr(int err, unsigned ini, unsigned end = 0);
};
} // namespace

#endif
