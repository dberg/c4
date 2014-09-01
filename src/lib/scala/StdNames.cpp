#include <string>
#include "c4/scala/TypeDefs.h"
#include "c4/scala/Names.h"
#include "c4/scala/StdNames.h"

namespace c4s {

/** Constructor */
KeywordSetBuilder::KeywordSetBuilder(Global *global): global(global) {}

spTermName KeywordSetBuilder::apply(std::string s) {
  spTermName result = global->names->newTermNameCached(s);
  kws.insert(result);
  return result;
}

std::set<spTermName> KeywordSetBuilder::result() {
  auto kwsCopy = kws;
  kws.clear();
  return kwsCopy;
}

/** Constructor */
Keywords::Keywords(Global *global) {
  auto kw = spKeywordSetBuilder(new KeywordSetBuilder(global));
  ABSTRACTkw  = kw->apply("abstract");
  CASEkw      = kw->apply("case");
  CLASSkw     = kw->apply("class");
  CATCHkw     = kw->apply("catch");
  DEFkw       = kw->apply("def");
  DOkw        = kw->apply("do");
  ELSEkw      = kw->apply("else");
  EXTENDSkw   = kw->apply("extends");
  FALSEkw     = kw->apply("false");
  FINALkw     = kw->apply("final");
  FINALLYkw   = kw->apply("finally");
  FORkw       = kw->apply("for");
  FORSOMEkw   = kw->apply("forSome");
  IFkw        = kw->apply("if");
  IMPLICITkw  = kw->apply("implicit");
  IMPORTkw    = kw->apply("import");
  LAZYkw      = kw->apply("lazy");
  MACROkw     = kw->apply("macro");
  MATCHkw     = kw->apply("match");
  NEWkw       = kw->apply("new");
  NULLkw      = kw->apply("null");
  OBJECTkw    = kw->apply("object");
  OVERRIDEkw  = kw->apply("override");
  PACKAGEkw   = kw->apply("package");
  PRIVATEkw   = kw->apply("private");
  PROTECTEDkw = kw->apply("protected");
  RETURNkw    = kw->apply("return");
  SEALEDkw    = kw->apply("sealed");
  SUPERkw     = kw->apply("super");
  THENkw      = kw->apply("then");
  THISkw      = kw->apply("this");
  THROWkw     = kw->apply("throw");
  TRAITkw     = kw->apply("trait");
  TRUEkw      = kw->apply("true");
  TRYkw       = kw->apply("try");
  TYPEkw      = kw->apply("type");
  VALkw       = kw->apply("val");
  VARkw       = kw->apply("var");
  WITHkw      = kw->apply("with");
  WHILEkw     = kw->apply("while");
  YIELDkw     = kw->apply("yield");
  DOTkw       = kw->apply(".");
  USCOREkw    = kw->apply("_");
  COLONkw     = kw->apply(":");
  EQUALSkw    = kw->apply("=");
  ARROWkw     = kw->apply("=>");
  LARROWkw    = kw->apply("<-");
  SUBTYPEkw   = kw->apply("<:");
  VIEWBOUNDkw = kw->apply("<%");
  SUPERTYPEkw = kw->apply(">:");
  HASHkw      = kw->apply("#");
  ATkw        = kw->apply("@");

  keywords = kw->result();

  // TODO:
  // auto javaKeywords = new JavaKeywords();
}

/** Constructor */
TermNames::TermNames() {}

/** Constructor */
nme::nme() {}

/** Constructor */
StdNames::StdNames(Global *global):
  keywords(spKeywords(new Keywords(global))) {}

/** Constructor */
const NameType CommonNames::EMPTY = "";
CommonNames::CommonNames() {}

} // namespace
