#include "djp/ScalaSyntaxHighlighting.h"

namespace djp {

// ----------------------------------------------------------------------------
// Public interface
// ----------------------------------------------------------------------------

void ScalaSyntaxHighlighting::build() {
  // Compilation Unit
  sh << "[";
  // TODO: std::vector<{‘package’ QualId semi}>
  if (compUnit->topStatSeq) {
    setTopStatSeq(compUnit->topStatSeq);
  }

  sh << "]";
}

std::string ScalaSyntaxHighlighting::get() {
  return sh.str();
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
void ScalaSyntaxHighlighting::setKeyword(spTokenNode tok) {
  sh << "(djp-sh-keyword " << (tok->ini + 1) << " " << (tok->end + 1) << ")";
}

// ----------------------------------------------------------------------------
// AST
// ----------------------------------------------------------------------------
void ScalaSyntaxHighlighting::setPackaging(spPackaging &packing) {
  // TODO:
}

void ScalaSyntaxHighlighting::setTmplDef(spTmplDef &tmplDef) {
  if (tmplDef->opt == TmplDef::Opt::CASE_CLASS) {
    // 'case'
    if (tmplDef->tokCase) {
      setKeyword(tmplDef->tokCase);
    }

    if (tmplDef->tokClass) {
      setKeyword(tmplDef->tokClass);
    }

    // TODO:
    //if (tmplDef->classDef) {
    //  setClassDef(tmplDef->classDef);
    //}
    return;
  }

  if (tmplDef->opt == TmplDef::Opt::CASE_OBJECT) {
    if (tmplDef->tokCase) {
      setKeyword(tmplDef->tokCase);
    }

    if (tmplDef->tokObject) {
      setKeyword(tmplDef->tokObject);
    }

    // TODO:
    //if (tmplDef->objectDef) {
    //  setObjectDef(tmplDef->objectDef);
    //}
    return;
  }

  if (tmplDef->opt == TmplDef::Opt::TRAIT) {
    if (tmplDef->tokTrait) {
      setKeyword(tmplDef->tokTrait);
    }

    // TODO:
    //if (tmplDef->traitDef) {
    //  setTraitDef(tmplDef->traitDef);
    //}

    return;
  }
}

void ScalaSyntaxHighlighting::setTopStat(spTopStat &topStat) {
  if (topStat->opt == TopStat::Opt::TMPL_DEF) {
    // TODO: {Annotation [nl]} {Modifier}
    if (topStat->tmplDef) {
      setTmplDef(topStat->tmplDef);
    }
    return;
  }

  if (topStat->opt == TopStat::Opt::IMPORT) {
    // TODO:
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGING) {
    if (topStat->packaging) {
      setPackaging(topStat->packaging);
    }
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGE_OBJECT) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setTopStatSeq(spTopStatSeq &topStatSeq) {
  if (topStatSeq->topStat) {
    setTopStat(topStatSeq->topStat);
  }

  // TODO: std::vector<{semi TopStat}>
}

} // namespace
