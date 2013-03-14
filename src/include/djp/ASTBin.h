//-*- C++ -*-
#ifndef __ASTBIN_H__
#define __ASTBIN_H__
#include <stdint.h> // cstdint
#include <boost/shared_ptr.hpp>

typedef uint32_t u4;
typedef uint16_t u2;
typedef boost::shared_ptr<struct ClassFile> spClassFile;

struct ClassFile {
  u4 magic;
  u2 minor_version;
  u2 major_version;
  // TODO:
  //u2             constant_pool_count;
  //cp_info        constant_pool[constant_pool_count-1];
  //u2             access_flags;
  //u2             this_class;
  //u2             super_class;
  //u2             interfaces_count;
  //u2             interfaces[interfaces_count];
  //u2             fields_count;
  //field_info     fields[fields_count];
  //u2             methods_count;
  //method_info    methods[methods_count];
  //u2             attributes_count;
  //attribute_info attributes[attributes_count];
};

#endif
