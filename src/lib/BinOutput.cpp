#include "djp/BinOutput.h"

namespace djp {

void BinOutput::build() {
  // TODO:
  out = "I'm a happy binary file.\n";
  out += "Magic: ";
  out += itos(parser.classFile->magic);
  out += "\n";
  out += "Minor: ";
  out += itos(parser.classFile->minor_version);
  out += "\n";
  out += "Major: ";
  out += itos(parser.classFile->major_version);
  out += "\n";
}

} // namespace
