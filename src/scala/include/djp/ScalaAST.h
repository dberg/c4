//-*- C++ -*-
#ifndef __SCALA_AST_H__
#define __SCALA_AST_H__
#include <memory>
#include "ScalaToken.h"

namespace djp {
namespace scala {

typedef std::shared_ptr<struct LexId> spLexId;

typedef std::shared_ptr<struct AnnotType> spAnnotType;
typedef std::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef std::shared_ptr<struct ClassParents> spClassParents;
typedef std::shared_ptr<struct ClassTemplate> spClassTemplate;
typedef std::shared_ptr<struct ClassTemplateOpt> spClassTemplateOpt;
typedef std::shared_ptr<struct Constr> spConstr;
typedef std::shared_ptr<struct ObjectDef> spObjectDef;
typedef std::shared_ptr<struct SimpleType> spSimpleType;
typedef std::shared_ptr<struct StableId> spStableId;
typedef std::shared_ptr<struct TemplateBody> spTemplateBody;
typedef std::shared_ptr<struct TemplateStat> spTemplateStat;
typedef std::shared_ptr<struct TmplDef> spTmplDef;
typedef std::shared_ptr<struct TokenNode> spTokenNode;
typedef std::shared_ptr<struct TopStat> spTopStat;
typedef std::shared_ptr<struct TopStatSeq> spTopStatSeq;

// ----------------------------------------------------------------------------
// Helper nodes
// ----------------------------------------------------------------------------

/**
 * Every node inherits from ASTBase which provides tracking utilities such as
 * the node position in the buffer and error information.
 */
struct ASTBase {
  // position in the buffer where this node starts and ends.
  unsigned long ini;
  unsigned long end;
  // if any error is found, err should be set to true and errIdx
  // is the index in the error messages.
  bool err;
  int errIdx;

  ASTBase() : ini(0), end(0), err(false), errIdx(0) {}
  void addErr(int _errIdx) { err = true; errIdx = _errIdx; }
};

/**
 * TokenNode tracks reserved words and symbols found in the Scala language
 * specification.
 */
struct TokenNode : ASTBase {
  STok tok;
};

// -----------------------------------------------------------------------------
//   Lexical grammar
// -----------------------------------------------------------------------------
struct LexId : ASTBase {
  std::string id;
};

// ----------------------------------------------------------------------------
// AST based in the Scala grammar
// ----------------------------------------------------------------------------

/**
 * AnnotType ::= SimpleType {Annotation}
 */
struct AnnotType : ASTBase {
  spSimpleType simpleType;
  // TODO: std::vector<Annotation> annotations;
};

/**
 * CompilationUnit ::= {‘package’ QualId semi} TopStatSeq
 */
struct CompilationUnit : ASTBase {
  // TODO:
  // std::vector<{‘package’ QualId semi}>
  spTopStatSeq topStatSeq;
};

/**
 * ClassParents ::= Constr {‘with’ AnnotType}
 */
struct ClassParents : ASTBase {
  spConstr constr;
  // TODO: std::vector<spTokenNode, spAnnotType> paTokNodeAnnotType;
};

/**
 * ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody]
 */
struct ClassTemplate : ASTBase {
  // TODO: spEarlyDefs earlyDefs;
  spClassParents classParents;
  // TODO: spTemplateBody tmplBody;
};

/**
 * ClassTemplateOpt ::= ‘extends’ ClassTemplate
 *                    | [[‘extends’] TemplateBody]
 */
struct ClassTemplateOpt : ASTBase {
  enum class Opt {
    UNDEFINED,
    CLASS_TEMPLATE,
    TEMPLATE_BODY,
  };

  Opt opt;
  spTokenNode tokExtends;
  spClassTemplate classTmpl;
  spTemplateBody tmplBody;

  ClassTemplateOpt() : opt(Opt::UNDEFINED) {}
};

/**
 * Constr ::= AnnotType {ArgumentExprs}
 */
struct Constr : ASTBase {
  spAnnotType annotType;
  // TODO: std::vector<spArgumentExprs> argExprs;
};

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
struct ObjectDef : ASTBase {
  spLexId lId;
  spClassTemplateOpt classTmplOpt;
};

/**
 * SimpleType ::= SimpleType TypeArgs
 *              | SimpleType ‘#’ id
 *              | StableId
 *              | Path ‘.’ ‘type’
 *              | ‘(’ Types ’)’
 */
struct SimpleType : ASTBase {
  enum class Opt {
    UNDEFINED,
    TYPE_ARGS,
    HASH_ID,
    STABLE_ID,
    PATH,
    TYPES,
  };

  Opt opt;

  // TODO: SimpleType TypeArgs
  // TODO: SimpleType '#' id
  spStableId stableId;
  // TODO: Path '.' 'type'
  // TODO: '(' Types ')'

  SimpleType() : opt(Opt::UNDEFINED) {}
};

/**
 * StableId ::= id
 *            | Path ‘.’ id
 *            | [id ’.’] ‘super’ [ClassQualifier] ‘.’ id
 */
struct StableId : ASTBase {
  enum class Opt {
    UNDEFINED,
    ID,
    PATH,
    SUPER,
  };

  Opt opt;
  spLexId id;
  // TODO: Path ‘.’ id
  // TODO: [id ’.’] ‘super’ [ClassQualifier] ‘.’ id

  StableId() : opt(Opt::UNDEFINED) {}
};

/**
 * TemplateBody ::= [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
 */
struct TemplateBody : ASTBase {
  spTokenNode lCurlyB;
  // TODO:
  //spSelfType selfType;
  spTemplateStat tmplStat;
  // TODO:
  //std::vector<pair<spSemi, spTemplateStat>> paSemiTmplStat;
  spTokenNode rCurlyB;
};

/**
 * TemplateStat ::= Import
 *                | {Annotation [nl]} {Modifier} Def
 *                | {Annotation [nl]} {Modifier} Dcl
 *                | Expr
 */
struct TemplateStat : ASTBase {
  enum class Opt {
    UNDEFINED,
    IMPORT,
    DEF,
    DCL,
    EXPR,
  };

  Opt opt;
  // TODO: spImport import;
  // TODO: std::vector<spAnnotation> annotations;
  // TODO: std::vector<spModifier> modifiers;
  // TODO: spDef def;
  // TODO: spExpr expr;

  TemplateStat() : opt(Opt::UNDEFINED) {}
};

/**
 * TmplDef ::= [‘case’] ‘class’ ClassDef
 *           | [‘case’] ‘object’ ObjectDef
 *           | ‘trait’ TraitDef
 */
struct TmplDef : ASTBase {
  enum class Opt {
    UNDEFINED,
    CASE_CLASS,
    CASE_OBJECT,
    TRAIT,
  };

  Opt opt;

  // shared
  spTokenNode tokCase;

  // OPT_CASE_CLASS
  spTokenNode tokClass;
  // TODO: spClassDef classDef;

  // OPT_CASE_OBJECT
  spTokenNode tokObject;
  spObjectDef objectDef;

  // OPT_TRAIT
  spTokenNode tokTrait;
  // TODO:
  //spTraitDef traitDef;

  TmplDef() : opt(Opt::UNDEFINED) {}
};

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
struct TopStat : ASTBase {
  enum Opt {
    UNDEFINED,
    ANNOTATION,
    IMPORT,
    PACKAGING,
    PACKAGE,
  };

  Opt opt;

  // TODO:
  // std::vector<{Annotation [nl]}>
  // std::vector<{Modifier}>
  spTmplDef tmplDef;

  TopStat() : opt(Opt::UNDEFINED) {}
};

/**
 * TopStatSeq ::= TopStat {semi TopStat}
 */
struct TopStatSeq : ASTBase {
  spTopStat topStat;
  // TODO:
  // std::vector<{semi TopStat}>
};

}} // namespace

#endif
