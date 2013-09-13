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
void ScalaSyntaxHighlighting::setAnnotType(spAnnotType &annotType) {
  if (annotType->simpleType) {
    setSimpleType(annotType->simpleType);
  }

  // TODO: std::vector<Annotation> annotations;
}

void ScalaSyntaxHighlighting::setClassParents(spClassParents &classParents) {
  if (classParents->constr) {
    setConstr(classParents->constr);
  }

  // TODO: std::vector<spTokenNode, spAnnotType> paTokNodeAnnotType;
}

void ScalaSyntaxHighlighting::setClassTemplate(spClassTemplate &classTmpl) {
  // TODO: spEarlyDefs earlyDefs;

  if (classTmpl->classParents) {
    setClassParents(classTmpl->classParents);
  }

  // TODO: spTemplateBody tmplBody;
}

void ScalaSyntaxHighlighting::setClassTemplateOpt(
  spClassTemplateOpt &classTmplOpt) {

  if (classTmplOpt->opt == ClassTemplateOpt::Opt::CLASS_TEMPLATE) {
    if (classTmplOpt->tokExtends) {
      setKeyword(classTmplOpt->tokExtends);
    }

    if (classTmplOpt->classTmpl) {
      setClassTemplate(classTmplOpt->classTmpl);
    }

    return;
  }

  if (classTmplOpt->opt == ClassTemplateOpt::Opt::TEMPLATE_BODY) {
    if (classTmplOpt->tokExtends) {
      setKeyword(classTmplOpt->tokExtends);
    }

    // TODO: tmplBody

    return;
  }
}

void ScalaSyntaxHighlighting::setConstr(spConstr &constr) {
  if (constr->annotType) {
    setAnnotType(constr->annotType);
  }

  // TODO:
  //std::vector<spArgumentExprs> argExprs;
}

void ScalaSyntaxHighlighting::setLexId(spLexId &id) {
  sh << "(djp-sh-identifier " << (id->ini + 1) << " " << (id->end + 1) << ")";
}

void ScalaSyntaxHighlighting::setObjectDef(spObjectDef &objectDef) {
  if (objectDef->id) {
    setLexId(objectDef->id);
  }

  if (objectDef->classTmplOpt) {
    setClassTemplateOpt(objectDef->classTmplOpt);
  }
}

void ScalaSyntaxHighlighting::setPackaging(spPackaging &packing) {
  // TODO:
}

void ScalaSyntaxHighlighting::setSimpleType(spSimpleType &simpleType) {
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

    if (tmplDef->objectDef) {
      setObjectDef(tmplDef->objectDef);
    }

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
