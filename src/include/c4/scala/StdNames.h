//-*- C++ -*-
#ifndef __C4_SCALA_STD_NAMES_H__
#define __C4_SCALA_STD_NAMES_H__

#include <set>
#include "c4/scala/Global.h"

namespace c4s {

class KeywordSetBuilder {
private:
  Global *global;
  std::set<TermName *> kws;

public:
  KeywordSetBuilder(Global *global);
  ~KeywordSetBuilder();
  TermName* apply(std::string s);
  std::set<TermName*> result();
};

class Keywords {

public:
  TermName *ABSTRACTkw;
  TermName *CASEkw;
  TermName *CLASSkw;
  TermName *CATCHkw;
  TermName *DEFkw;
  TermName *DOkw;
  TermName *ELSEkw;
  TermName *EXTENDSkw;
  TermName *FALSEkw;
  TermName *FINALkw;
  TermName *FINALLYkw;
  TermName *FORkw;
  TermName *FORSOMEkw;
  TermName *IFkw;
  TermName *IMPLICITkw;
  TermName *IMPORTkw;
  TermName *LAZYkw;
  TermName *MACROkw;
  TermName *MATCHkw;
  TermName *NEWkw;
  TermName *NULLkw;
  TermName *OBJECTkw;
  TermName *OVERRIDEkw;
  TermName *PACKAGEkw;
  TermName *PRIVATEkw;
  TermName *PROTECTEDkw;
  TermName *RETURNkw;
  TermName *SEALEDkw;
  TermName *SUPERkw;
  TermName *THENkw;
  TermName *THISkw;
  TermName *THROWkw;
  TermName *TRAITkw;
  TermName *TRUEkw;
  TermName *TRYkw;
  TermName *TYPEkw;
  TermName *VALkw;
  TermName *VARkw;
  TermName *WITHkw;
  TermName *WHILEkw;
  TermName *YIELDkw;
  TermName *DOTkw;
  TermName *USCOREkw;
  TermName *COLONkw;
  TermName *EQUALSkw;
  TermName *ARROWkw;
  TermName *LARROWkw;
  TermName *SUBTYPEkw;
  TermName *VIEWBOUNDkw;
  TermName *SUPERTYPEkw;
  TermName *HASHkw;
  TermName *ATkw;

  std::set<TermName*> keywords;

  Keywords(Global *global);
  ~Keywords();
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
  Keywords *keywords;
  StdNames(Global *global);
  ~StdNames();
};

class CommonNames {
public:
  static const NameType EMPTY;
  CommonNames();
};

} // namespace

#endif
