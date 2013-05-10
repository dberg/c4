#include "djp/BinOutput.h"

namespace djp {

void BinOutput::build() {
  buildHeader();
  buildConstantPool();
  buildClassInfo();
}

void BinOutput::buildHeader() {
  out << "Magic: 0x"
    // add hex and uppercase
    << std::uppercase << std::hex
    << parser.classFile->magic << std::endl
    // remove hex and uppercase
    << std::nouppercase << std::dec
    << "Minor: " << parser.classFile->minor_version << std::endl
    << "Major: " << parser.classFile->major_version << std::endl << std::endl;
}

void BinOutput::buildConstantPool() {
  u2 poolCount = parser.classFile->constant_pool_count;
  out << "Constant Pool size: " << poolCount << std::endl;

  std::vector<spCPItem> &items = parser.classFile->constant_pool->items;
  for (u2 i = 1; i < poolCount; i++) {
    spCPItem item = items[i];
    out << "#" << i << " " << tags[item->tag];
    switch (item->tag) {
      case CONSTANT_Class:
        buildCPClassInfo(item->cClassInfo);
        break;
      case CONSTANT_Fieldref:
        buildCPFieldrefInfo(item->cFieldrefInfo);
        break;
      case CONSTANT_Methodref:
        buildCPMethodrefInfo(item->cMethodrefInfo);
        break;
      case CONSTANT_InterfaceMethodref:
        buildCPInterfaceMethodrefInfo(item->cInterfaceMethodrefInfo);
        break;
      case CONSTANT_String:
        buildCPStringInfo(item->cStringInfo);
        break;
      case CONSTANT_Integer:
        buildCPIntegerInfo(item->cIntegerInfo);
        break;
      case CONSTANT_Float:
        buildCPFloatInfo(item->cFloatInfo);
        break;
      case CONSTANT_Long:
        buildCPLongInfo(item->cLongInfo);
        break;
      case CONSTANT_Double:
        buildCPDoubleInfo(item->cDoubleInfo);
        break;
      case CONSTANT_NameAndType:
        buildCPNameAndTypeInfo(item->cNameAndTypeInfo);
        break;
      case CONSTANT_Utf8:
        buildUtf8Info(item->cUtf8Info);
        break;
      case CONSTANT_MethodHandle:
        buildMethodHandleInfo(item->cMethodHandleInfo);
        break;
      case CONSTANT_MethodType:
        buildCPMethodTypeInfo(item->cMethodTypeInfo);
        break;
      case CONSTANT_InvokeDynamic:
        buildCPInvokeDynamicInfo(item->cInvokeDynamicInfo);
        break;
    }
    out << std::endl;
  }

  out << std::endl;
}

void BinOutput::buildCPClassInfo(spCClassInfo& cClassInfo) {
  out << " name_index #" << cClassInfo->name_index;
}

void BinOutput::buildCPFieldrefInfo(spCFieldrefInfo& cFieldrefInfo) {
  out << " class_index #" << cFieldrefInfo->class_index
    << " name_and_type_index #" << cFieldrefInfo->name_and_type_index;
}

void BinOutput::buildCPMethodrefInfo(spCMethodrefInfo& cMethodrefInfo) {
  out << " class_index #" << cMethodrefInfo->class_index
    << " name_and_type_index #" << cMethodrefInfo->name_and_type_index;
}

void BinOutput::buildCPInterfaceMethodrefInfo(
  spCInterfaceMethodrefInfo& cInterfaceMethodrefInfo) {
  out << " class_index #" << cInterfaceMethodrefInfo->class_index
    << " name_and_type_index #"
    << cInterfaceMethodrefInfo->name_and_type_index;
}

void BinOutput::buildCPStringInfo(spCStringInfo& cStringInfo) {
  out << " string_index #" << cStringInfo->string_index;
}

void BinOutput::buildCPIntegerInfo(spCIntegerInfo& cIntegerInfo) {
  out << " bytes " << cIntegerInfo->bytes;
}

void BinOutput::buildCPFloatInfo(spCFloatInfo& cFloatInfo) {
  out << " bytes " << cFloatInfo->bytes;
}

void BinOutput::buildCPLongInfo(spCLongInfo& cLongInfo) {
  out << " high_bytes " << cLongInfo->high_bytes
    << " low_bytes " << cLongInfo->low_bytes;
}

void BinOutput::buildCPDoubleInfo(spCDoubleInfo& cDoubleInfo) {
  out << " high_bytes " << cDoubleInfo->high_bytes
    << " low_bytes " << cDoubleInfo->low_bytes;
}

void BinOutput::buildCPNameAndTypeInfo(spCNameAndTypeInfo& cNameAndTypeInfo) {
  out << " name_index #" << cNameAndTypeInfo->name_index
    << " descriptor_index #" << cNameAndTypeInfo->descriptor_index;
}

void BinOutput::buildUtf8Info(spCUtf8Info& cUtf8Info) {
  out << " " << std::string(cUtf8Info->bytes.begin(), cUtf8Info->bytes.end());
}

void BinOutput::buildMethodHandleInfo(spCMethodHandleInfo& cMethodHandleInfo) {
  out << " reference_kind #" << cMethodHandleInfo->reference_kind
    << " reference_index #" << cMethodHandleInfo->reference_index;
}

void BinOutput::buildCPMethodTypeInfo(spCMethodTypeInfo& cMethodTypeInfo) {
  out << " descriptor_index #" << cMethodTypeInfo->descriptor_index;
}

void BinOutput::buildCPInvokeDynamicInfo(
  spCInvokeDynamicInfo& cInvokeDynamicInfo) {
  out << " bootstrap_method_attr_index #"
    << cInvokeDynamicInfo->bootstrap_method_attr_index
    << " name_and_type_index #"
    << cInvokeDynamicInfo->name_and_type_index;
}

void BinOutput::buildClassInfo() {
  for (auto it = classModifiers.begin(); it != classModifiers.end(); it++) {
    if (it->first & parser.classFile->access_flags) {
      out << it->second << " ";
    }
  }

  out << "this_class #" << parser.classFile->this_class
    << " super_class #" << parser.classFile->super_class
    << std::endl << std::endl;
}

} // namespace
