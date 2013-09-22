//-*- C++ -*-
#ifndef __BIN_OUTPUT_H__
#define __BIN_OUTPUT_H__
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include "c4/ASTBin.h"
#include "c4/BinOutputCode.h"
#include "c4/Opcodes.h"
#include "c4/ParserBin.h"

namespace c4 {

class BinOutput {
  ParserBin &parser;
  std::unordered_map<int, std::string> tags;
  std::unordered_map<int, std::string> classModifiers;
  std::unordered_map<int, std::string> methodFlags;
  std::unordered_map<int, std::string> fieldFlags;

  void buildHeader();

  void buildConstantPool();
  void buildCPClassInfo(spCClassInfo& cClassInfo);
  void buildCPFieldrefInfo(spCFieldrefInfo& cFieldrefInfo);
  void buildCPMethodrefInfo(spCMethodrefInfo& cMethodrefInfo);
  void buildCPInterfaceMethodrefInfo(
    spCInterfaceMethodrefInfo& cInterfaceMethodrefInfo);
  void buildCPStringInfo(spCStringInfo& cStringInfo);
  void buildCPIntegerInfo(spCIntegerInfo& cIntegerInfo);
  void buildCPFloatInfo(spCFloatInfo& cFloatInfo);
  void buildCPLongInfo(spCLongInfo& cLongInfo);
  void buildCPDoubleInfo(spCDoubleInfo& cDoubleInfo);
  void buildCPNameAndTypeInfo(spCNameAndTypeInfo& cNameAndTypeInfo);
  void buildUtf8Info(spCUtf8Info& cUtf8Info);
  void buildMethodHandleInfo(spCMethodHandleInfo& cMethodHandleInfo);
  void buildCPMethodTypeInfo(spCMethodTypeInfo& cMethodTypeInfo);
  void buildCPInvokeDynamicInfo(spCInvokeDynamicInfo& cInvokeDynamicInfo);

  void buildClassInfo();
  void buildInterfaces();
  void buildFields();
  void buildMethods();
  void buildMethod(spMethodInfo &method);
  void buildAttributeInfo(spAttributeInfo &attribute);
  void buildAttributes();
  void buildAttributes(std::vector<spAttributeInfo> &attributes);
  void buildAttributeCode(spAttributeInfo &attribute);
  void buildCodeAttribute(spCodeAttribute &code);
  void buildCode(std::vector<u1> &code);

  void buildAttributeSourceFile(spAttributeInfo &attribute);
  void buildAttributeLineNumberTable(spAttributeInfo &attribute);

public:
  BinOutput(ParserBin &parser) : parser(parser) {
    tags = {
      { CONSTANT_Class, "Class" },
      { CONSTANT_Fieldref, "Fieldref" },
      { CONSTANT_Methodref, "Methodref" },
      { CONSTANT_InterfaceMethodref, "InterfaceMethodref" },
      { CONSTANT_String, "String" },
      { CONSTANT_Integer, "Integer" },
      { CONSTANT_Float, "Float" },
      { CONSTANT_Long, "Long" },
      { CONSTANT_Double, "Double" },
      { CONSTANT_NameAndType, "NameAndType" },
      { CONSTANT_Utf8, "Utf8" },
      { CONSTANT_MethodHandle, "MethodHandle" },
      { CONSTANT_MethodType, "MethodType" },
      { CONSTANT_InvokeDynamic, "InvokeDynamic"}
    };

    classModifiers = {
      { CLASS_ACC_PUBLIC, "public" },
      { CLASS_ACC_FINAL, "final" },
      { CLASS_ACC_SUPER, "super" },
      { CLASS_ACC_INTERFACE, "interface" },
      { CLASS_ACC_ABSTRACT, "abstract" },
      { CLASS_ACC_SYNTHETIC, "synthetic" },
      { CLASS_ACC_ANNOTATION, "annotation" },
      { CLASS_ACC_ENUM, "enum" },
    };

    methodFlags = {
      { METHOD_ACC_PUBLIC, "public" },
      { METHOD_ACC_PRIVATE, "private" },
      { METHOD_ACC_PROTECTED, "protected" },
      { METHOD_ACC_STATIC, "static" },
      { METHOD_ACC_FINAL, "final" },
      { METHOD_ACC_SYNCHRONIZED, "synchronized" },
      { METHOD_ACC_BRIDGE, "bridge" },
      { METHOD_ACC_VARARGS, "varargs" },
      { METHOD_ACC_NATIVE, "native" },
      { METHOD_ACC_ABSTRACT, "abstract" },
      { METHOD_ACC_STRICT, "strict" },
      { METHOD_ACC_SYNTHETIC, "synthetic" },
    };

    fieldFlags = {
      { FIELD_ACC_PUBLIC, "public" },
      { FIELD_ACC_PRIVATE, "private" },
      { FIELD_ACC_PROTECTED, "protected" },
      { FIELD_ACC_STATIC, "static" },
      { FIELD_ACC_FINAL, "final" },
      { FIELD_ACC_VOLATILE, "volatile" },
      { FIELD_ACC_TRANSIENT, "transient" },
      { FIELD_ACC_SYNTHETIC, "synthetic" },
      { FIELD_ACC_ENUM, "enum" },
    };
  }

  void build();
  std::stringstream out;
};

} // namespace

#endif
