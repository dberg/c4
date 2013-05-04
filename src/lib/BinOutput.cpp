#include "djp/BinOutput.h"

namespace djp {

void BinOutput::build() {
  buildHeader();
}

void BinOutput::buildHeader() {
  out << "Magic: 0x"
    // add hex and uppercase
    << std::uppercase << std::hex
    << parser.classFile->magic << std::endl
    // remove hex and uppercase
    << std::nouppercase << std::dec
    << "Minor: " << parser.classFile->minor_version << std::endl
    << "Major: " << parser.classFile->major_version << std::endl;
}

} // namespace
