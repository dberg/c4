//-*- C++ -*-
#ifndef __PARSERBIN_H__
#define __PARSERBIN_H__
#include <stdint.h> // cstdint
#include <string>
#include <vector>
#include "ASTBin.h"

namespace djp {

class ParserBin {

  enum ParserBinErr {
    ERR_INVALID_MAGIC_NUMBER,
    ERR_INVALID_CONST_POOL_TAG,
  };

  unsigned pos; // current buffer position
  int err;
  const std::string filename;
  std::vector<unsigned char> buffer;

  void addErr(int errCode) { err = errCode; }
  u1 getU1();
  u2 getU2();
  u4 getU4();
  void parseClassFile();
  void parseConstantPool(unsigned poolCount, spCPInfo &constantPool);
  void parseCClass(spCPItem &item);
  void parseCFieldref(spCPItem &item);
  void parseCPMethodref(spCPItem &item);
  void parseCPInterfaceMethodref(spCPItem &item);
  void parseCPString(spCPItem &item);
  void parseCPInteger(spCPItem &item);
  void parseCPFloat(spCPItem &item);
  void parseCPLong(spCPItem &item);
  void parseCPDouble(spCPItem &item);
  void parseCPNameAndTypeInfo(spCPItem &item);
  void parseCPUtf8(spCPItem &item);
  void parseCPMethodHandle(spCPItem &item);
  void parseCPMethodType(spCPItem &item);
  void parseCPInvokeDynamic(spCPItem &item);
  void parseInterfaces(u2 interfaces_count);
  void parseFields(u2 fields_count);
  void parseMethods(u2 methods_count);
  void parseAttributes(u2 attributesCount,
    std::vector<spAttributeInfo> &attributes);

  // Helpers
  AttributeType getAttributeType(u2 attributeNameIndex);
  void parseCodeAttribute(spCodeAttribute &code);
  void parseLineNumberTable(spLineNumberTable &table);

public:
  ParserBin(const std::string filename,
    const std::vector<unsigned char> &buffer)
    : pos(0), err(0), filename(filename), buffer(buffer) {}

  spClassFile classFile;

  void parse();
};

} // namespace

#endif
