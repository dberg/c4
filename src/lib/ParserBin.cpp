#include "djp/ParserBin.h"

namespace djp {

// -----------------------------------------------------------------------------
// Public interface
// -----------------------------------------------------------------------------
void ParserBin::parse() {
  parseClassFile();
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------
unsigned ParserBin::getU2() {
  uint16_t u2 = (buffer[pos++] << 8) | buffer[pos++];
  return u2;
}

unsigned ParserBin::getU4() {
  uint32_t u4 = (buffer[pos++] << 24)
    | (buffer[pos++] << 16)
    | (buffer[pos++] << 8)
    | buffer[pos++];
  return u4;
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

/// ClassFile {
///        u4             magic;
///        u2             minor_version;
///        u2             major_version;
///        u2             constant_pool_count;
///        cp_info        constant_pool[constant_pool_count-1];
///        u2             access_flags;
///        u2             this_class;
///        u2             super_class;
///        u2             interfaces_count;
///        u2             interfaces[interfaces_count];
///        u2             fields_count;
///        field_info     fields[fields_count];
///        u2             methods_count;
///        method_info    methods[methods_count];
///        u2             attributes_count;
///        attribute_info attributes[attributes_count];
/// }
void ParserBin::parseClassFile() {
  classFile = spClassFile(new ClassFile);
  classFile->magic = getU4();
  if (classFile->magic != 0xCAFEBABE) {
    addErr(ERR_INVALID_MAGIC_NUMBER);
    return;
  }

  classFile->minor_version = getU2();
  classFile->major_version = getU2();
}

} // namespace
