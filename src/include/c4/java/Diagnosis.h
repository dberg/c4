//-*- C++ -*-
#ifndef __C4_JAVA_DIAGNOSIS_H__
#define __C4_JAVA_DIAGNOSIS_H__

#include <memory>
#include <vector>

#include "c4/common/ErrorCodes.h"

namespace c4 {

class Diagnosis;
typedef std::shared_ptr<Diagnosis> spDiagnosis;

class Diagnosis {
public:
  std::vector<spError> errors;

  int addErr(int err, unsigned ini, unsigned end = 0);
};
} // namespace

#endif
