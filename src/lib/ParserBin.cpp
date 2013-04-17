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
  classFile->access_flags = getU2();
  classFile->this_class = getU2();
  classFile->super_class = getU2();

  u2 interfaces_count = getU2();
  classFile->interfaces_count = interfaces_count;
  parseInterfaces(interfaces_count);

  u2 fields_count = getU2();
  classFile->fields_count = fields_count;
  parseFields(fields_count);

  u2 methods_count = getU2();
  classFile->methods_count = methods_count;
  parseMethods(methods_count);

  u2 attributes_count = getU2();
  classFile->attributes_count = attributes_count;
  parseAttributes(attributes_count, classFile->attributes);
}

void ParserBin::parseConstantPool(unsigned poolCount, spCPInfo &constantPool) {
  // The constant pool index is not zero based so we create a vector with
  // size equals to the poolCount and not poolCount - 1. The first entry in the
  // vector is a dummy value.
  unsigned entries = poolCount;
  constantPool->items.reserve(entries);
  constantPool->items.push_back(spCPItem(new CPItem)); // dummy value

  for (unsigned i = 1; i < entries; i++) {
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
        parseCPInterfaceMethodref(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_String:
        parseCPString(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Integer:
        parseCPInteger(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Float:
        parseCPFloat(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Long:
        parseCPLong(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_Double:
        parseCPDouble(item);
        constantPool->items.push_back(item);
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
        parseCPMethodHandle(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_MethodType:
        parseCPMethodType(item);
        constantPool->items.push_back(item);
        break;

      case CONSTANT_InvokeDynamic:
        parseCPInvokeDynamic(item);
        constantPool->items.push_back(item);
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

void ParserBin::parseCPInterfaceMethodref(spCPItem &item) {
  item->cInterfaceMethodrefInfo = spCInterfaceMethodrefInfo(
    new CInterfaceMethodrefInfo);
  item->cInterfaceMethodrefInfo->class_index = getU2();
  item->cInterfaceMethodrefInfo->name_and_type_index = getU2();
}

void ParserBin::parseCPString(spCPItem &item) {
  item->cStringInfo = spCStringInfo(new CStringInfo);
  item->cStringInfo->string_index = getU2();
}

void ParserBin::parseCPInteger(spCPItem &item) {
  item->cIntegerInfo = spCIntegerInfo(new CIntegerInfo);
  item->cIntegerInfo->bytes = getU4();
}

void ParserBin::parseCPFloat(spCPItem &item) {
  item->cFloatInfo = spCFloatInfo(new CFloatInfo);
  item->cFloatInfo->bytes = getU4();
}

void ParserBin::parseCPLong(spCPItem &item) {
  item->cLongInfo = spCLongInfo(new CLongInfo);
  item->cLongInfo->high_bytes = getU4();
  item->cLongInfo->low_bytes = getU4();
}

void ParserBin::parseCPDouble(spCPItem &item) {
  item->cDoubleInfo = spCDoubleInfo(new CDoubleInfo);
  item->cDoubleInfo->high_bytes = getU4();
  item->cDoubleInfo->low_bytes = getU4();
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

void ParserBin::parseCPMethodHandle(spCPItem &item) {
  item->cMethodHandleInfo = spCMethodHandleInfo(new CMethodHandleInfo);
  item->cMethodHandleInfo->reference_kind = getU1();
  item->cMethodHandleInfo->reference_index = getU2();
}

void ParserBin::parseCPMethodType(spCPItem &item) {
  item->cMethodTypeInfo = spCMethodTypeInfo(new CMethodTypeInfo);
  item->cMethodTypeInfo->descriptor_index = getU2();
}

void ParserBin::parseCPInvokeDynamic(spCPItem &item) {
  item->cInvokeDynamicInfo = spCInvokeDynamicInfo(new CInvokeDynamicInfo);
  item->cInvokeDynamicInfo->bootstrap_method_attr_index = getU2();
  item->cInvokeDynamicInfo->name_and_type_index = getU2();
}

void ParserBin::parseInterfaces(u2 interfaces_count) {
  if (!interfaces_count) {
    return;
  }

  classFile->interfaces.reserve(interfaces_count);
  for (unsigned i = 0; i < interfaces_count; i++) {
    classFile->interfaces.push_back(getU2());
  }
}

void ParserBin::parseFields(u2 fields_count) {
  for (unsigned i = 0; i < fields_count; i++) {
    spFieldInfo field = spFieldInfo(new FieldInfo);
    field->access_flags = getU2();
    field->name_index = getU2();
    field->descriptor_index = getU2();
    u2 attributesCount = getU2();
    field->attributes_count = attributesCount;
    parseAttributes(attributesCount, field->attributes);
    classFile->fields.push_back(field);
  }
}

void ParserBin::parseMethods(u2 methods_count) {
  for (unsigned i = 0; i < methods_count; i++) {
    spMethodInfo method = spMethodInfo(new MethodInfo);
    method->access_flags = getU2();
    method->name_index = getU2();
    method->descriptor_index = getU2();
    u2 attributesCount = getU2();
    method->attributes_count = attributesCount;
    parseAttributes(attributesCount, method->attributes);
    classFile->methods.push_back(method);
  }
}

void ParserBin::parseAttributes(u2 attributesCount,
  std::vector<spAttributeInfo> &attributes) {

  attributes.reserve(attributesCount);
  for (unsigned i = 0; i < attributesCount; i++) {
    spAttributeInfo info = spAttributeInfo(new AttributeInfo);
    info->attribute_name_index = getU2();
    u4 length = getU4();
    info->attribute_length = length;
    info->info.reserve(length);
    for (unsigned j = 0; j < length; j++) {
      info->info.push_back(getU1());
    }

    attributes.push_back(info);
  }
}

} // namespace
