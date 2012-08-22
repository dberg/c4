#include "Diagnosis.h"

namespace djp {

void Diagnosis::addError(int ini, int end, int err) {
  spError error = spError(new Error(ini, end, err));
  errors.push_back(error);
}

} // namespace
