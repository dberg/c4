#include "Diagnosis.h"

namespace djp {

int Diagnosis::addError(int ini, int end, int err) {
  spError error = spError(new Error(ini, end, err));
  int idx = errors.size();
  errors.push_back(error);
  return idx;
}

} // namespace
