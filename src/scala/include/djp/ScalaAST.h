//-*- C++ -*-
#ifndef __SCALA_AST_H__
#define __SCALA_AST_H__
#include <memory>

namespace djp {
namespace scala {

typedef std::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef std::shared_ptr<struct ObjectDef> spObjectDef;
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

// ----------------------------------------------------------------------------
// AST based in the Scala grammar
// ----------------------------------------------------------------------------

/**
 * CompilationUnit ::= {‘package’ QualId semi} TopStatSeq
 */
struct CompilationUnit : ASTBase {
  // TODO:
  // std::vector<{‘package’ QualId semi}>
  spTopStatSeq topStatSeq;
};

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
struct ObjectDef : ASTBase {
  // TODO:
};

/**
 * TmplDef ::= [‘case’] ‘class’ ClassDef
 *           | [‘case’] ‘object’ ObjectDef
 *           | ‘trait’ TraitDef
 */
struct TmplDef : ASTBase {
  enum TmplDefOpt {
    OPT_UNDEFINED,
    OPT_CASE_CLASS,
    OPT_CASE_OBJECT,
    OPT_TRAIT,
  };

  TmplDefOpt opt;

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

  TmplDef() : opt(OPT_UNDEFINED) {}
};

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
struct TopStat : ASTBase {
  enum TopStatOpt {
    OPT_UNDEFINED,
    OPT_ANNOTATION,
    OPT_IMPORT,
    OPT_PACKAGING,
    OPT_PACKAGE,
  };

  TopStatOpt opt;

  // TODO:
  // std::vector<{Annotation [nl]}
  // std::vector<{Modifier}
  spTmplDef tmplDef;

  TopStat() : opt(OPT_UNDEFINED) {}
};

/**
 * TopStatSeq ::= TopStat {semi TopStat}
 */
struct TopStatSeq : ASTBase {
  spTopStat topStat;
  // TODO:
  // std::vector<{semi TopStat}
};

}} // namespace

#endif
