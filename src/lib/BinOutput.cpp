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
}

void BinOutput::buildCPClassInfo(spCClassInfo& cClassInfo) {
  out << " TODO";
}

void BinOutput::buildCPFieldrefInfo(spCFieldrefInfo& cFieldrefInfo) {
  out << " TODO";
}

void BinOutput::buildCPMethodrefInfo(spCMethodrefInfo& cMethodrefInfo) {
  out << " TODO";
}

void BinOutput::buildCPInterfaceMethodrefInfo(
  spCInterfaceMethodrefInfo& cInterfaceMethodrefInfo) {
  out << " TODO";
}

void BinOutput::buildCPStringInfo(spCStringInfo& cStringInfo) {
  out << " TODO";
}

void BinOutput::buildCPIntegerInfo(spCIntegerInfo& cIntegerInfo) {
  out << " TODO";
}

void BinOutput::buildCPFloatInfo(spCFloatInfo& cFloatInfo) {
  out << " TODO";
}

void BinOutput::buildCPLongInfo(spCLongInfo& cLongInfo) {
  out << " TODO";
}

void BinOutput::buildCPDoubleInfo(spCDoubleInfo& cDoubleInfo) {
  out << " TODO";
}

void BinOutput::buildCPNameAndTypeInfo(spCNameAndTypeInfo& cNameAndTypeInfo) {
  out << " TODO";
}

void BinOutput::buildUtf8Info(spCUtf8Info& cUtf8Info) {
  out << " TODO";
}

void BinOutput::buildMethodHandleInfo(spCMethodHandleInfo& cMethodHandleInfo) {
  out << " TODO";
}

void BinOutput::buildCPMethodTypeInfo(spCMethodTypeInfo& cMethodTypeInfo) {
  out << " TODO";
}

void BinOutput::buildCPInvokeDynamicInfo(
  spCInvokeDynamicInfo& cInvokeDynamicInfo) {
  out << " TODO";
}

} // namespace
