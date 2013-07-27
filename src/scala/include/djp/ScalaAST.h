//-*- C++ -*-
#ifndef __SCALA_AST_H__
#define __SCALA_AST_H__
#include <memory>
#include <vector>
#include <utility>
#include "ScalaToken.h"

namespace djp {
namespace scala {

typedef std::shared_ptr<struct LexId> spLexId;

typedef std::shared_ptr<struct AnnotType> spAnnotType;
typedef std::shared_ptr<struct ArgumentExprs> spArgumentExprs;
typedef std::shared_ptr<struct Block> spBlock;
typedef std::shared_ptr<struct BlockExpr> spBlockExpr;
typedef std::shared_ptr<struct BlockStat> spBlockStat;
typedef std::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef std::shared_ptr<struct ClassParents> spClassParents;
typedef std::shared_ptr<struct ClassTemplate> spClassTemplate;
typedef std::shared_ptr<struct ClassTemplateOpt> spClassTemplateOpt;
typedef std::shared_ptr<struct Constr> spConstr;
typedef std::shared_ptr<struct Expr1> spExpr1;
typedef std::shared_ptr<struct InfixExpr> spInfixExpr;
typedef std::shared_ptr<struct ObjectDef> spObjectDef;
typedef std::shared_ptr<struct Path> spPath;
typedef std::shared_ptr<struct PrefixExpr> spPrefixExpr;
typedef std::shared_ptr<struct PostfixExpr> spPostfixExpr;
typedef std::shared_ptr<struct Semi> spSemi;
typedef std::shared_ptr<struct SimpleExpr> spSimpleExpr;
typedef std::shared_ptr<struct SimpleExpr1> spSimpleExpr1;
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
 * Block ::= {BlockStat semi} [ResultExpr]
 */
struct Block : ASTBase {
  std::vector<std::pair<spBlockStat, spSemi>> paBlockStatSemi;
  // TODO: ResultExpr
};

/**
 * BlockExpr ::= ‘{’ CaseClauses ‘}’
 *             | ‘{’ Block ‘}’
 */
struct BlockExpr : ASTBase {
  enum class Opt {
    UNDEFINED,
    CASE,
    BLOCK,
  };

  Opt opt;
  spTokenNode tokLCurlyB;
  // TODO: ‘{’ CaseClauses ‘}’
  spBlock block;
  spTokenNode tokRCurlyB;

  BlockExpr() : opt(Opt::UNDEFINED) {}
};

/**
 * BlockStat ::= Import
 *             | {Annotation} [‘implicit’ | ‘lazy’] Def
 *             | {Annotation} {LocalModifier} TmplDef
 *             | Expr1
 */
struct BlockStat : ASTBase {
  enum class Opt {
    UNDEFINED,
    DEF,
    TMPL_DEF,
    EXPR1,
  };

  Opt opt;

  // TODO: Import
  // TODO: {Annotation} [‘implicit’ | ‘lazy’] Def
  // TODO: {Annotation} {LocalModifier} TmplDef
  spExpr1 expr1;

  BlockStat() : opt(Opt::UNDEFINED) {}
};


/**
 * ArgumentExprs ::= ‘(’ [Exprs] ‘)’
 *                 | ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ’)’
 *                 | [nl] BlockExpr
 */
struct ArgumentExprs : ASTBase {
  enum class Opt {
    UNDEFINED,
    EXPRS,
    EXPRS_POSTFIX_EXPR,
    BLOCK_EXPR,
  };

  Opt opt;
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: ‘(’ [Exprs ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ’)’
  spBlockExpr blockExpr;

  ArgumentExprs() : opt(Opt::UNDEFINED) {}
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
  std::vector<spArgumentExprs> argExprs;
};

/**
 * Expr1 ::= ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
 *         | ‘while’ ‘(’ Expr ‘)’ {nl} Expr
 *         | ‘try’ ‘{’ Block ‘}’ [‘catch’ ‘{’ CaseClauses ‘}’]
 *           [‘finally’ Expr]
 *         | ‘do’ Expr [semi] ‘while’ ‘(’ Expr ’)’
 *         | ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’)
 *           {nl} [‘yield’] Expr
 *         | ‘throw’ Expr
 *         | ‘return’ [Expr]
 *         | [SimpleExpr ‘.’] id ‘=’ Expr
 *         | SimpleExpr1 ArgumentExprs ‘=’ Expr
 *         | PostfixExpr
 *         | PostfixExpr Ascription
 *         | PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
 */
struct Expr1 : ASTBase {
  enum class Opt {
    UNDEFINED,
    IF,
    WHILE,
    TRY,
    DO,
    FOR,
    THROW,
    RETURN,
    ID_EQUALS_EXPR,
    SIMPLE_EXPR1,
    POSTFIX_EXPR,
    POSTFIX_EXPR_ASCRIPTION,
    POSTFIX_EXPR_MATCH,
  };

  Opt opt;

  // TODO: ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
  // TODO: ‘while’ ‘(’ Expr ‘)’ {nl} Expr
  // TODO: ‘try’ ‘{’ Block ‘}’ [‘catch’ ‘{’ CaseClauses ‘}’] [‘finally’ Expr]
  // TODO: ‘do’ Expr [semi] ‘while’ ‘(’ Expr ’)’
  // TODO: ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’) {nl} [‘yield’] Expr
  // TODO: ‘throw’ Expr
  // TODO: ‘return’ [Expr]
  // TODO: [SimpleExpr ‘.’] id ‘=’ Expr
  // TODO: SimpleExpr1 ArgumentExprs ‘=’ Expr
  spPostfixExpr postfixExpr;
  // TODO: PostfixExpr Ascription
  // TODO: PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’

  Expr1() : opt(Opt::UNDEFINED) {}
};

/**
 * InfixExpr ::= PrefixExpr
 *             | InfixExpr id [nl] InfixExpr
 */
struct InfixExpr : ASTBase {
  enum class Opt {
    UNDEFINED,
    PREFIX,
    INFIX,
  };

  Opt opt;
  spPrefixExpr prefixExpr;
  // TODO: InfixExpr id [nl] InfixExpr

  InfixExpr() : opt(Opt::UNDEFINED) {}
};

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
struct ObjectDef : ASTBase {
  spLexId id;
  spClassTemplateOpt classTmplOpt;
};

/**
 * Path ::= StableId
 *        | [id ‘.’] ‘this’
 */
struct Path : ASTBase {
  enum class Opt {
    UNDEFINED,
    STABLE_ID,
    THIS,
  };

  Opt opt;
  spStableId stableId;
  // TODO: [id ‘.’] ‘this’

  Path() : opt(Opt::UNDEFINED) {}
};

/**
 * PrefixExpr ::= [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
 */
struct PrefixExpr : ASTBase {
  spTokenNode tok;
  spSimpleExpr simpleExpr;
};

/**
 * PostfixExpr ::= InfixExpr [id [nl]]
 */
struct PostfixExpr : ASTBase {
  spInfixExpr infixExpr;
  // TODO: [id [nl]]
};

/**
 * semi ::= ‘;’ | nl {nl}
 */
struct Semi : ASTBase {
  enum class Opt {
    UNDEFINED,
    SEMI_COLON,
    NL,
  };

  Opt opt;
  spTokenNode tokSemiColon;

  Semi() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleExpr ::= ‘new’ (ClassTemplate | TemplateBody)
 *              | BlockExpr
 *              | SimpleExpr1 [‘_’]
 */
struct SimpleExpr : ASTBase {
  enum class Opt {
    UNDEFINED,
    BLOCK_EXPR,
    SIMPLE_EXPR1,
  };

  // TODO: ‘new’ (ClassTemplate | TemplateBody)
  spBlockExpr blockExpr;
  spSimpleExpr1 simpleExpr1;
  // TODO: [‘_’]

  Opt opt;

  SimpleExpr() : opt(Opt::UNDEFINED) {}
};

/**
 * Literal
 * Path
 * ‘_’
 * ‘(’ [Exprs] ‘)’
 * SimpleExpr ‘.’ id
 * SimpleExpr TypeArgs
 * SimpleExpr1 ArgumentExprs
 * XmlExpr
 */
struct SimpleExpr1 : ASTBase {
  enum class Opt {
    UNDEFINED,
    LITERAL,
    PATH,
    UNDERSCORE,
    EXPRS,
    SIMPLEEXPR_ID,
    SIMPLEEXPR_TYPEARGS,
    SIMPLEEXPR1_ARGUMENTEXPRS,
    XMLEXPR,
  };

  Opt opt;
  // TODO: Literal

  // Path
  spPath path;

  // TODO: ‘_’
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: SimpleExpr ‘.’ id
  // TODO: SimpleExpr TypeArgs
  // TODO: SimpleExpr1 ArgumentExprs
  // TODO: XmlExpr

  SimpleExpr1() : opt(Opt::UNDEFINED) {}
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
