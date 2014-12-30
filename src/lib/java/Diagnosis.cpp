#include <memory>
#include "c4/java/Diagnosis.h"

using std::make_shared;

namespace c4j {

/**
 * Add Diagnosis error that can be used by parser clients.
 * @param err: ErrorCode
 * @param ini: initial position in the buffer
 * @param end: end position in the buffer
 */
int Diagnosis::addErr(int err, unsigned ini, unsigned end) {
  if (end == 0) { end = ini + 1; }
  auto error = make_shared<c4::Error>(err, ini, end);
  int idx = errors.size();
  errors.push_back(error);
  return idx;
}

} // namespace
