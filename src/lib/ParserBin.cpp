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
     switch (tag) {
      case CONSTANT_Class:
        // TODO:
        break;

      case CONSTANT_Fieldref:
        // TODO:
        break;

      case CONSTANT_Methodref:
        // TODO:
        break;

      case CONSTANT_InterfaceMethodref:
        // TODO:
        break;

      case CONSTANT_String:
        // TODO:
        break;

      case CONSTANT_Integer:
        // TODO:
        break;

      case CONSTANT_Float:
        // TODO:
        break;

      case CONSTANT_Long:
        // TODO:
        break;

      case CONSTANT_Double:
        // TODO:
        break;

      case CONSTANT_NameAndType:
        // TODO:
        break;

      case CONSTANT_Utf8:
        // TODO:
        break;

      case CONSTANT_MethodHandle:
        // TODO:
        break;

      case CONSTANT_MethodType:
        // TODO:
        break;

      case CONSTANT_InvokeDynamic:
        // TODO:
        break;

      default:
        addErr(ERR_INVALID_CONST_POOL_TAG);
    }
  }
}

} // namespace
