#include "djp/BinOutput.h"

namespace djp {

void BinOutput::build() {
  buildHeader();
  buildConstantPool();
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

void BinOutput::buildConstantPool() {
  u2 poolCount = parser.classFile->constant_pool_count;
  out << "Constant Pool size: " << poolCount << std::endl;

  std::vector<spCPItem> &items = parser.classFile->constant_pool->items;
  for (u2 i = 1; i < poolCount; i++) {
    spCPItem item = items[i];
    out << "#" << i << " " << tags[item->tag] << std::endl;
  }
}

} // namespace
