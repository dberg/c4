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
u1 ParserBin::getU1() {
  uint8_t u1 = buffer[pos++];
  return u1;
}

u2 ParserBin::getU2() {
  uint16_t u2 = (buffer[pos++] << 8) | buffer[pos++];
  return u2;
}

u4 ParserBin::getU4() {
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
  classFile->constant_pool_count = getU2();
  parseConstantPool(classFile->constant_pool_count, classFile->constant_pool);
}

void ParserBin::parseConstantPool(unsigned poolCount,
  std::vector<spCPInfo> &constantPool) {

  classFile->constant_pool.reserve(poolCount - 1);
  for (unsigned i = 0; i < poolCount; i++) {
    u1 tag = getU1();
    // TODO:
    switch (tag) {
      case CONSTANT_Class:
        std::cout << "CONSTANT_Class\n";
        break;

      case CONSTANT_Fieldref:
        std::cout << "CONSTANT_Fieldref\n";
        break;

      case CONSTANT_Methodref:
        std::cout << "CONSTANT_Methodref\n";
        break;

      case CONSTANT_InterfaceMethodref:
        std::cout << "CONSTANT_InterfaceMethodref\n";
        break;

      case CONSTANT_String:
        std::cout << "CONSTANT_String\n";
        break;

      case CONSTANT_Integer:
        std::cout << "CONSTANT_Integer\n";
        break;

      case CONSTANT_Float:
        std::cout << "CONSTANT_Float\n";
        break;

      case CONSTANT_Long:
        std::cout << "CONSTANT_Long\n";
        break;

      case CONSTANT_Double:
        std::cout << "CONSTANT_Double\n";
        break;

      case CONSTANT_NameAndType:
        std::cout << "CONSTANT_NameAndType\n";
        break;

      case CONSTANT_Utf8:
        std::cout << "CONSTANT_Utf8\n";
        break;

      case CONSTANT_MethodHandle:
        std::cout << "CONSTANT_MethodHandle\n";
        break;

      case CONSTANT_MethodType:
        std::cout << "CONSTANT_MethodType\n";
        break;

      case CONSTANT_InvokeDynamic:
        std::cout << "CONSTANT_InvokeDynamic\n";
        break;

      default:
        std::cout << "Unknown: " << tag << "\n";
    }
  }
}

} // namespace
