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
Keywords::Keywords(Global *global): global(global) {
  auto kw = new spKeywordsSetBuilder(new KeywordsSetBuilder(global));
  spTermName ABSTRACTkw  = kw.apply("abstract");
  spTermName CASEkw      = kw.apply("case");
  spTermName CLASSkw     = kw.apply("class");
  spTermName CATCHkw     = kw.apply("catch");
  spTermName DEFkw       = kw.apply("def");
  spTermName DOkw        = kw.apply("do");
  spTermName ELSEkw      = kw.apply("else");
  spTermName EXTENDSkw   = kw.apply("extends");
  spTermName FALSEkw     = kw.apply("false");
  spTermName FINALkw     = kw.apply("final");
  spTermName FINALLYkw   = kw.apply("finally");
  spTermName FORkw       = kw.apply("for");
  spTermName FORSOMEkw   = kw.apply("forSome");
  spTermName IFkw        = kw.apply("if");
  spTermName IMPLICITkw  = kw.apply("implicit");
  spTermName IMPORTkw    = kw.apply("import");
  spTermName LAZYkw      = kw.apply("lazy");
  spTermName MACROkw     = kw.apply("macro");
  spTermName MATCHkw     = kw.apply("match");
  spTermName NEWkw       = kw.apply("new");
  spTermName NULLkw      = kw.apply("null");
  spTermName OBJECTkw    = kw.apply("object");
  spTermName OVERRIDEkw  = kw.apply("override");
  spTermName PACKAGEkw   = kw.apply("package");
  spTermName PRIVATEkw   = kw.apply("private");
  spTermName PROTECTEDkw = kw.apply("protected");
  spTermName RETURNkw    = kw.apply("return");
  spTermName SEALEDkw    = kw.apply("sealed");
  spTermName SUPERkw     = kw.apply("super");
  spTermName THENkw      = kw.apply("then");
  spTermName THISkw      = kw.apply("this");
  spTermName THROWkw     = kw.apply("throw");
  spTermName TRAITkw     = kw.apply("trait");
  spTermName TRUEkw      = kw.apply("true");
  spTermName TRYkw       = kw.apply("try");
  spTermName TYPEkw      = kw.apply("type");
  spTermName VALkw       = kw.apply("val");
  spTermName VARkw       = kw.apply("var");
  spTermName WITHkw      = kw.apply("with");
  spTermName WHILEkw     = kw.apply("while");
  spTermName YIELDkw     = kw.apply("yield");
  spTermName DOTkw       = kw.apply(".");
  spTermName USCOREkw    = kw.apply("_");
  spTermName COLONkw     = kw.apply(":");
  spTermName EQUALSkw    = kw.apply("=");
  spTermName ARROWkw     = kw.apply("=>");
  spTermName LARROWkw    = kw.apply("<-");
  spTermName SUBTYPEkw   = kw.apply("<:");
  spTermName VIEWBOUNDkw = kw.apply("<%");
  spTermName SUPERTYPEkw = kw.apply(">:");
  spTermName HASHkw      = kw.apply("#");
  spTermName ATkw        = kw.apply("@");

  std::set<spTermName> keywords = kw.result();

  // TODO:
  // auto javaKeywords = new JavaKeywords();
}

/** Constructor */
TermNames::TermNames() {}

/** Constructor */
nme::nme() {}

/** Constructor */
StdNames::StdNames() {}

} // namespace
