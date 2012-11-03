#include "Diagnosis.h"

namespace djp {

int Diagnosis::addErr(int err, unsigned ini, unsigned end) {
  if (end == 0) { end = ini + 1; }
  spError error = spError(new Error(err, ini, end));
  int idx = errors.size();
  errors.push_back(error);
  return idx;
}

} // namespace
