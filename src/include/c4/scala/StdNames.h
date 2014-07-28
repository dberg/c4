//-*- C++ -*-
#ifndef __C4_SCALA_STD_NAMES_H__
#define __C4_SCALA_STD_NAMES_H__

#include <set>
#include "c4/scala/Global.h"

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

public:
  spTermName ABSTRACTkw;
  spTermName CASEkw;
  spTermName CLASSkw;
  spTermName CATCHkw;
  spTermName DEFkw;
  spTermName DOkw;
  spTermName ELSEkw;
  spTermName EXTENDSkw;
  spTermName FALSEkw;
  spTermName FINALkw;
  spTermName FINALLYkw;
  spTermName FORkw;
  spTermName FORSOMEkw;
  spTermName IFkw;
  spTermName IMPLICITkw;
  spTermName IMPORTkw;
  spTermName LAZYkw;
  spTermName MACROkw;
  spTermName MATCHkw;
  spTermName NEWkw;
  spTermName NULLkw;
  spTermName OBJECTkw;
  spTermName OVERRIDEkw;
  spTermName PACKAGEkw;
  spTermName PRIVATEkw;
  spTermName PROTECTEDkw;
  spTermName RETURNkw;
  spTermName SEALEDkw;
  spTermName SUPERkw;
  spTermName THENkw;
  spTermName THISkw;
  spTermName THROWkw;
  spTermName TRAITkw;
  spTermName TRUEkw;
  spTermName TRYkw;
  spTermName TYPEkw;
  spTermName VALkw;
  spTermName VARkw;
  spTermName WITHkw;
  spTermName WHILEkw;
  spTermName YIELDkw;
  spTermName DOTkw;
  spTermName USCOREkw;
  spTermName COLONkw;
  spTermName EQUALSkw;
  spTermName ARROWkw;
  spTermName LARROWkw;
  spTermName SUBTYPEkw;
  spTermName VIEWBOUNDkw;
  spTermName SUPERTYPEkw;
  spTermName HASHkw;
  spTermName ATkw;

  std::set<spTermName> keywords;

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
  spKeywords keywords;

  StdNames(Global *global);
};

} // namespace

#endif
