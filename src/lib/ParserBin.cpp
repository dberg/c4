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
  classFile->constant_pool = spCPInfo(new CPInfo);
  parseConstantPool(classFile->constant_pool_count, classFile->constant_pool);
}

void ParserBin::parseConstantPool(unsigned poolCount, spCPInfo &constantPool) {
  constantPool->items.reserve(poolCount - 1);
  for (unsigned i = 0; i < poolCount; i++) {
    u1 tag = getU1();
    spCPItem item = spCPItem(new CPItem);
    item->tag = tag;
    switch (tag) {
      case CONSTANT_Class:
        parseCClass(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Fieldref:
        parseCFieldref(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Methodref:
        parseCPMethodref(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_InterfaceMethodref:
        // TODO:
        break;

      case CONSTANT_String:
        parseCPString(item);
        constantPool->items.push_back(item);
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
        parseCPNameAndTypeInfo(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Utf8:
        parseCPUtf8(item);
        constantPool->items.push_back(item);
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

void ParserBin::parseCClass(spCPItem &item) {
  item->cClassInfo = spCClassInfo(new CClassInfo);
  item->cClassInfo->name_index = getU2();
}

void ParserBin::parseCFieldref(spCPItem &item) {
  item->cFieldrefInfo = spCFieldrefInfo(new CFieldrefInfo);
  item->cFieldrefInfo->class_index = getU2();
  item->cFieldrefInfo->name_and_type_index = getU2();
}

void ParserBin::parseCPMethodref(spCPItem &item) {
  item->cMethodrefInfo = spCMethodrefInfo(new CMethodrefInfo);
  item->cMethodrefInfo->class_index = getU2();
  item->cMethodrefInfo->name_and_type_index = getU2();
}

void ParserBin::parseCPString(spCPItem &item) {
  item->cStringInfo = spCStringInfo(new CStringInfo);
  item->cStringInfo->string_index = getU2();
}

void ParserBin::parseCPNameAndTypeInfo(spCPItem &item) {
  item->cNameAndTypeInfo = spCNameAndTypeInfo(new CNameAndTypeInfo);
  item->cNameAndTypeInfo->name_index = getU2();
  item->cNameAndTypeInfo->descriptor_index = getU2();
}

void ParserBin::parseCPUtf8(spCPItem &item) {
  item->cUtf8Info = spCUtf8Info(new CUtf8Info);
  u2 length = getU2();
  item->cUtf8Info->length = length;
  for (unsigned i = 0; i < length; i++) {
    item->cUtf8Info->bytes.push_back(getU1());
  }
}

} // namespace
