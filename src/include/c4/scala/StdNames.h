//-*- C++ -*-
#ifndef __C4_SCALA_STD_NAMES_H__
#define __C4_SCALA_STD_NAMES_H__

namespace c4s {

class KeywordSetBuilder {
private:
  Global *global;
  std::set<spTermName> kws;

public:
  KeywordSetBuilder(Global *global);
  spTermName apply(std::string s);
  std::set<spTermName> result();
};

class Keywords {
private:
  Global *global;

public:
  Keywords(Global *global);
};

class TermNames {
public:
  TermNames();
};

class nme {
public:
  nme();
};

class StdNames {
public:
  StdNames();
};

} // namespace

#endif
