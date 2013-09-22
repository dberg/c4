#include "c4/BinOutput.h"

namespace c4 {

void BinOutput::build() {
  buildHeader();
  buildConstantPool();
  buildClassInfo();
  buildInterfaces();
  buildFields();
  buildMethods();
  buildAttributes();
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

void BinOutput::buildMethods() {
  out << "Methods count " << parser.classFile->methods_count
    << std::endl << std::endl;

  std::vector<spMethodInfo> &methods = parser.classFile->methods;
  for (u2 i = 0; i < methods.size(); i++) {
    buildMethod(methods[i]);
  }
}

void BinOutput::buildMethod(spMethodInfo &method) {
  for (auto it = classModifiers.begin(); it != classModifiers.end(); it++) {
    if (it->first & method->access_flags) {
      out << it->second << " ";
    }
  }

  out << "name_index #" << method->name_index
    << " descriptor_index #" << method->descriptor_index
    << " attributes_count #" << method->attributes_count
    << std::endl;

  buildAttributes(method->attributes);
}

/**
 * Class attributes
 */
void BinOutput::buildAttributes() {
  out << "attributes_count " << parser.classFile->attributes_count
    << std::endl << std::endl;
  buildAttributes(parser.classFile->attributes);
}

void BinOutput::buildAttributes(std::vector<spAttributeInfo> &attributes) {
  for (u2 i = 0; i < attributes.size(); i++) {
    buildAttributeInfo(attributes[i]);
  }
}

void BinOutput::buildAttributeInfo(spAttributeInfo &attribute) {
  switch (attribute->type) {
    case ATTRIBUTE_TYPE_UNKNOWN:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_CONSTANT_VALUE:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_CODE:
      buildAttributeCode(attribute);
      break;
    case ATTRIBUTE_TYPE_STACK_MAP_TABLE:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_EXCEPTIONS:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_INNER_CLASSES:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_ENCLOSING_METHOD:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_SYNTHETIC:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_SIGNATURE:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_SOURCE_FILE:
      buildAttributeSourceFile(attribute);
      break;
    case ATTRIBUTE_TYPE_SOURCE_DEBUG_EXTENSION:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_LINE_NUMBER_TABLE:
      buildAttributeLineNumberTable(attribute);
      break;
    case ATTRIBUTE_TYPE_LOCAL_VARIABLE_TABLE:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_LOCAL_VARIABLE_TYPE_TABLE:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_DEPRECATED:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_RUNTIME_VISIBLE_ANNOTATIONS:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_RUNTIME_INVISIBLE_ANNOTATIONS:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_RUNTIME_VISIBLE_PARAMETER_ANNOTATIONS:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_RUNTIME_INVISIBLE_PARAMETER_ANNOTATIONS:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_ANNOTATION_DEFAULT:
      // TODO:
      break;
    case ATTRIBUTE_TYPE_BOOTSTRAP_METHODS:
      // TODO:
      break;
  }
}

void BinOutput::buildAttributeCode(spAttributeInfo &attribute) {
  out << "attribute_name_index #" << attribute->attribute_name_index
    << " attribute_length " << attribute->attribute_length << std::endl;
  buildCodeAttribute(attribute->code);
}

void BinOutput::buildCodeAttribute(spCodeAttribute &code) {
  // code
  out << "max_stack " << code->max_stack
    << " max_locals " << code->max_locals << std::endl;
  buildCode(code->code);

  // exceptions
  if (code->exception_table_length) {
    out << "  Exception table:" << std::endl;
    out << "    from    to      target  type" << std::endl;
    for (auto ex: code->exceptions) {
      out << "    "
        << std::left << std::setw(8) << std::setfill(' ') << ex->start_pc
        << std::left << std::setw(8) << std::setfill(' ') << ex->end_pc
        << std::left << std::setw(8) << std::setfill(' ') << ex->handler_pc
        << "#"
        << std::left << std::setw(7) << std::setfill(' ') << ex->catch_type
        << std::endl;
    }
  }

  // attributes
  out << "  ";
  buildAttributes(code->attributes);
}

void BinOutput::buildCode(std::vector<u1> &code) {
  BinOutputCode outCode(code, out);
  outCode.build();
}

void BinOutput::buildInterfaces() {
  const u2 count = parser.classFile->interfaces_count;
  out << "Interfaces count " <<  count << std::endl;
  for (auto constantPoolIdx: parser.classFile->interfaces) {
    out << "  #" << constantPoolIdx;
  }

  if (count) {
    out << std::endl;
  }

  out << std::endl;
}

void BinOutput::buildFields() {
  u2 count = parser.classFile->fields_count;
  out << "Fields count " << count << std::endl;
  for (auto &field: parser.classFile->fields) {
    // field access and property flags
    if (count) { out << " "; }
    for (auto flag: fieldFlags) {
      if (flag.first & field->access_flags) {
        out << " " << flag.second;
      }
    }

    out << " name_index #" << field->name_index
      << " descriptor_index #" << field->descriptor_index
      << " attributes_count " << field->attributes_count << std::endl;

    buildAttributes(field->attributes);
  }

  out << std::endl;
}

void BinOutput::buildAttributeSourceFile(spAttributeInfo &attribute) {
  out << "SourceFile"
    << " attribute_name_index #" << attribute->attribute_name_index
    << " attribute_length " << attribute->attribute_length
    << " sourcefile_index #" << attribute->sourcefile_index << std::endl;
}

void BinOutput::buildAttributeLineNumberTable(spAttributeInfo &attribute) {
  out << "LineNumberTable"
    << " attribute_name_index #" << attribute->attribute_name_index
    << " attribute_length " << attribute->attribute_length
    << " line_number_table_length "
    << attribute->table->line_number_table_length << std::endl;

  for (auto row: attribute->table->table) {
    out << "    line " << row->line_number << ": " << row->start_pc << std::endl;
  }

  out << std::endl;
}

} // namespace
