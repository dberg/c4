//-*- C++ -*-
#ifndef __BIN_OUTPUT_H__
#define __BIN_OUTPUT_H__
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include "djp/ASTBin.h"
#include "djp/ParserBin.h"

namespace djp {

class BinOutput {
  ParserBin &parser;
  std::unordered_map<int, std::string> tags;
  std::unordered_map<int, std::string> classModifiers;

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
  }

  void build();
  std::stringstream out;
};

} // namespace

#endif
