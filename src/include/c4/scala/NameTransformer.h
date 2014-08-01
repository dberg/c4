//-*- C++ -*-
#ifndef __C4_SCALA_NAME_TRANSFORMER_H__
#define __C4_SCALA_NAME_TRANSFORMER_H__

namespace c4s {

/** Provides functions to encode and decode Scala symbolic names. */
class NameTransformer {
public:
  NameTransformer();

  std::string encode(std::string name);
};

} // namespace

#endif
