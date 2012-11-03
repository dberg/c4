//-*- C++ -*-
#ifndef __DIAGNOSIS_H__
#define __DIAGNOSIS_H__
#include <vector>
#include <boost/shared_ptr.hpp>
#include "ErrorCodes.h"

namespace djp {

class Diagnosis;
typedef boost::shared_ptr<Diagnosis> spDiagnosis;

class Diagnosis {
public:
  std::vector<spError> errors;
  //TODO: std::vector<spWarning> warnings;

  int addErr(int err, unsigned ini, unsigned end = 0);
};
} // namespace

#endif
