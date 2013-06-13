//-*- C++ -*-
#ifndef __DIAGNOSIS_H__
#define __DIAGNOSIS_H__
#include <memory>
#include <vector>
#include "ErrorCodes.h"

namespace djp {

class Diagnosis;
typedef std::shared_ptr<Diagnosis> spDiagnosis;

class Diagnosis {
public:
  std::vector<spError> errors;

  int addErr(int err, unsigned ini, unsigned end = 0);
};
} // namespace

#endif
