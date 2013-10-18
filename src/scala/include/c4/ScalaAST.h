//-*- C++ -*-
#ifndef __SCALA_AST_H__
#define __SCALA_AST_H__
#include <memory>
#include <vector>
#include <utility>
#include "ScalaToken.h"

/**
 * We have applied a few modifications in the grammar.
 *
 * 1) Remove left recursion in SimpleExpr1
 *
 * SimpleExpr1 ::= SimpleExpr1Head SimpleExpr1Tail | SimpleExpr1Head
 *
 * SimpleExpr1Head ::= Literal
 *                   | Path
 *                   | '_'
 *                   | '(' [Exprs] ')'
 *                   | SimpleExpr '.' id
 *                   | SimpleExpr TypeArgs
 *                   | XmlExpr
 *
 * SimpleExpr1Tail ::=  ArgumentExprs SimpleExpr1Tail | ArgumentExprs
 *
 * 2) Remove left recursion between Path and StableId. We first remove
 *    Path references in StableId while creating IdPeriod and PeriodId
 *
 * IdPeriod ::= id '.'
 * PeriodId ::= '.' id
 *
 * StableId ::= id
 *            | StableId PeriodId
 *            | [IdPeriod] 'this' PeriodId
 *            | [IdPeriod] 'super' [ClassQualifier] PeriodId
 *
 * and then we remove the StableId left recursion:
 *
 * StableId ::= StableIdHead StableIdTail | StableIdHead
 *
 * StableIdHead ::= id
 *                | [IdPeriod] 'this' PeriodId
 *                | [IdPeriod] 'super' [ClassQualifier] PeriodId
 *
 * StableIdTail ::= PeriodId StableIdTail | PeriodId
 *
 * 3) Remove left recursion in SimpleType.
 *
 * SimpleType ::= SimpleTypeHead SimpleTypeTails | SimpleTypeHead
 *
 * SimpleTypeHead ::= StableId
 *                  | Path '.' 'type'
 *                  | '(' Types ')'
 *
 * SimpleTypeTails ::= SimpleTypeTail SimpleTypeTails | SimpleTypeTail
 *
 * SimpleTypeTail ::= TypeArgs | '#' id
 *
 * 4) Fix ImportExpr
 *
 * ImportExpr ::= QualId [ ('.' '_') | '.' ImportSelectors ]
 */

namespace c4s {

typedef struct ASTBase LexId;
typedef std::shared_ptr<LexId> spLexId;

// Comments
typedef struct ASTBase Comment;
typedef std::shared_ptr<Comment> spComment;

// Literals
typedef struct ASTBase StringLiteral;
typedef std::shared_ptr<StringLiteral> spStringLiteral;
typedef struct ASTBase IntegerLiteral;
typedef std::shared_ptr<IntegerLiteral> spIntegerLiteral;
typedef struct ASTBase FloatingPointLiteral;
typedef std::shared_ptr<FloatingPointLiteral> spFloatingPointLiteral;

typedef std::shared_ptr<struct IdPeriod> spIdPeriod;
typedef struct IdPeriod PeriodId;
typedef std::shared_ptr<PeriodId> spPeriodId;

typedef std::shared_ptr<struct Annotation> spAnnotation;;
typedef std::shared_ptr<struct AnnotType> spAnnotType;
typedef std::shared_ptr<struct ArgumentExprs> spArgumentExprs;
typedef std::shared_ptr<struct Block> spBlock;
typedef std::shared_ptr<struct BlockExpr> spBlockExpr;
typedef std::shared_ptr<struct BlockStat> spBlockStat;
typedef std::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef std::shared_ptr<struct CompoundType> spCompoundType;
typedef std::shared_ptr<struct ClassDef> spClassDef;
typedef std::shared_ptr<struct ClassParents> spClassParents;
typedef std::shared_ptr<struct ClassTemplate> spClassTemplate;
typedef std::shared_ptr<struct ClassTemplateOpt> spClassTemplateOpt;
typedef std::shared_ptr<struct Constr> spConstr;
typedef std::shared_ptr<struct Def> spDef;
typedef std::shared_ptr<struct Expr> spExpr;
typedef std::shared_ptr<struct Expr1> spExpr1;
typedef std::shared_ptr<struct Exprs> spExprs;
typedef std::shared_ptr<struct FunDef> spFunDef;
typedef std::shared_ptr<struct FunSig> spFunSig;
typedef std::shared_ptr<struct Import> spImport;
typedef std::shared_ptr<struct ImportExpr> spImportExpr;
typedef std::shared_ptr<struct ImportSelector> spImportSelector;
typedef std::shared_ptr<struct ImportSelectors> spImportSelectors;
typedef std::shared_ptr<struct InfixExpr> spInfixExpr;
typedef std::shared_ptr<struct InfixType> spInfixType;
typedef std::shared_ptr<struct Literal> spLiteral;
typedef std::shared_ptr<struct ObjectDef> spObjectDef;
typedef std::shared_ptr<struct Packaging> spPackaging;
typedef std::shared_ptr<struct Param> spParam;
typedef std::shared_ptr<struct Params> spParams;
typedef std::shared_ptr<struct ParamClause> spParamClause;
typedef std::shared_ptr<struct ParamClauses> spParamClauses;
typedef std::shared_ptr<struct ParamType> spParamType;
typedef std::shared_ptr<struct Path> spPath;
typedef std::shared_ptr<struct PrefixExpr> spPrefixExpr;
typedef std::shared_ptr<struct PostfixExpr> spPostfixExpr;
typedef std::shared_ptr<struct QualId> spQualId;
typedef std::shared_ptr<struct Semi> spSemi;
typedef std::shared_ptr<struct SimpleExpr> spSimpleExpr;
typedef std::shared_ptr<struct SimpleExpr1> spSimpleExpr1;
typedef std::shared_ptr<struct SimpleExpr1Head> spSimpleExpr1Head;
typedef std::shared_ptr<struct SimpleExpr1Tail> spSimpleExpr1Tail;
typedef std::shared_ptr<struct SimpleType> spSimpleType;
typedef std::shared_ptr<struct SimpleTypeHead> spSimpleTypeHead;
typedef std::shared_ptr<struct SimpleTypeTail> spSimpleTypeTail;
typedef std::shared_ptr<struct SimpleTypeTails> spSimpleTypeTails;
typedef std::shared_ptr<struct StableId> spStableId;
typedef std::shared_ptr<struct StableIdHead> spStableIdHead;
typedef std::shared_ptr<struct StableIdTail> spStableIdTail;
typedef std::shared_ptr<struct TemplateBody> spTemplateBody;
typedef std::shared_ptr<struct TemplateStat> spTemplateStat;
typedef std::shared_ptr<struct TmplDef> spTmplDef;
typedef std::shared_ptr<struct TokenNode> spTokenNode;
typedef std::shared_ptr<struct TopStat> spTopStat;
typedef std::shared_ptr<struct TopStatSeq> spTopStatSeq;
typedef std::shared_ptr<struct TraitDef> spTraitDef;
typedef std::shared_ptr<struct TraitParents> spTraitParents;
typedef std::shared_ptr<struct TraitTemplate> spTraitTemplate;
typedef std::shared_ptr<struct TraitTemplateOpt> spTraitTemplateOpt;
typedef std::shared_ptr<struct Type> spType;


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
  std::string val;

  ASTBase() : ini(0), end(0), err(false), errIdx(0), val("") {}
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
 * Annotation ::= '@' SimpleType {ArgumentExprs}
 */
struct Annotation : ASTBase {
  spTokenNode tokAt;
  spSimpleType simpleType;
  std::vector<spArgumentExprs> argExprsVec;
};

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
 * BlockExpr ::= '{' CaseClauses '}'
 *             | '{' Block '}'
 */
struct BlockExpr : ASTBase {
  enum class Opt {
    UNDEFINED,
    CASE,
    BLOCK,
  };

  Opt opt;
  spTokenNode tokLCurlyB;
  // TODO: '{' CaseClauses '}'
  spBlock block;
  spTokenNode tokRCurlyB;

  BlockExpr() : opt(Opt::UNDEFINED) {}
};

/**
 * BlockStat ::= Import
 *             | {Annotation} ['implicit' | 'lazy'] Def
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
  // TODO: {Annotation} ['implicit' | 'lazy'] Def
  // TODO: {Annotation} {LocalModifier} TmplDef
  spExpr1 expr1;

  BlockStat() : opt(Opt::UNDEFINED) {}
};


/**
 * ArgumentExprs ::= '(' [Exprs] ')'
 *                 | '(' [Exprs ','] PostfixExpr ':' '_' '*' ')'
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
  spTokenNode tokLParen;
  spTokenNode tokRParen;
  spTokenNode tokComma;
  spExprs exprs;
  spPostfixExpr postfixExpr;
  // TODO: ':' '_' '*'
  // TODO: [nl]
  spBlockExpr blockExpr;

  ArgumentExprs() : opt(Opt::UNDEFINED) {}
};

/**
 * CompilationUnit ::= {'package' QualId semi} TopStatSeq
 */
struct CompilationUnit : ASTBase {
  std::vector<std::tuple<spTokenNode, spQualId, spSemi>> tuples;
  spTopStatSeq topStatSeq;
};

/**
 * CompoundType ::= AnnotType {'with' AnnotType} [Refinement]
 *                | Refinement
 */
struct CompoundType : ASTBase {
  enum class Opt {
    UNDEFINED,
    ANNOT_TYPE,
    REFINEMENT,
  };

  Opt opt;

  // TODO: AnnotType {'with' AnnotType} [Refinement]
  spAnnotType annotType;

  // TODO: Refinement

  CompoundType() : opt(Opt::UNDEFINED) {}
};

/**
 * ClassDef ::= id [TypeParamClause] {ConstrAnnotation} [AccessModifier]
 *              ClassParamClauses ClassTemplateOpt
 */
struct ClassDef : ASTBase {
  spLexId id;
  // TODO: [TypeParamClause]
  // TODO: {ConstrAnnotation}
  // TODO: [AccessModifier]
  // TODO: ClassParamClauses
  spClassTemplateOpt classTmplOpt;
};

/**
 * ClassParents ::= Constr {'with' AnnotType}
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
 * ClassTemplateOpt ::= 'extends' ClassTemplate
 *                    | [['extends'] TemplateBody]
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
  std::vector<spArgumentExprs> argExprsVec;
};

/**
 * Def ::= PatVarDef
 *       | 'def' FunDef
 *       | 'type' {nl} TypeDef
 *       | TmplDef
 */
struct Def : ASTBase {
  enum class Opt {
    UNDEFINED,
    PAT_VAR_DEF,
    DEF,
    TYPE,
    TMPL_DEF,
  };

  Opt opt;

  // TODO: PatVarDef

  // 'def' FunDef
  spTokenNode tokDef;
  spFunDef funDef;

  // TODO: 'type' {nl} TypeDef
  // TODO: TmplDef

  Def() : opt(Opt::UNDEFINED) {}
};

/**
 * Expr ::= (Bindings | ['implicit'] id | '_') '=>' Expr
 *        | Expr1
 */
struct Expr : ASTBase {
  enum class Opt {
    UNDEFINED,
    EXPR,
    EXPR1,
  };

  Opt opt;
  // TODO: (Bindings | ['implicit'] id | '_') '=>' Expr
  spExpr1 expr1;

  Expr() : opt(Opt::UNDEFINED) {}
};

/**
 * Expr1 ::= 'if' '(' Expr ')' {nl} Expr [[semi] else Expr]
 *         | 'while' '(' Expr ')' {nl} Expr
 *         | 'try' '{' Block '}' ['catch' '{' CaseClauses '}']
 *           ['finally' Expr]
 *         | 'do' Expr [semi] 'while' '(' Expr ')'
 *         | 'for' ('(' Enumerators ')' | '{' Enumerators '}')
 *           {nl} ['yield'] Expr
 *         | 'throw' Expr
 *         | 'return' [Expr]
 *         | [SimpleExpr '.'] id '=' Expr
 *         | SimpleExpr1 ArgumentExprs '=' Expr
 *         | PostfixExpr
 *         | PostfixExpr Ascription
 *         | PostfixExpr 'match' '{' CaseClauses '}'
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

  // TODO: 'if' '(' Expr ')' {nl} Expr [[semi] else Expr]
  // TODO: 'while' '(' Expr ')' {nl} Expr
  // TODO: 'try' '{' Block '}' ['catch' '{' CaseClauses '}'] ['finally' Expr]
  // TODO: 'do' Expr [semi] 'while' '(' Expr ')'
  // TODO: 'for' ('(' Enumerators ')' | '{' Enumerators '}') {nl} ['yield'] Expr
  // TODO: 'throw' Expr
  // TODO: 'return' [Expr]

  // [SimpleExpr '.'] id '=' Expr
  spSimpleExpr simpleExpr;
  spTokenNode tokPeriod;
  spLexId id;
  spTokenNode tokEquals;
  spExpr expr;

  // TODO: SimpleExpr1 ArgumentExprs '=' Expr
  spPostfixExpr postfixExpr;
  // TODO: PostfixExpr Ascription
  // TODO: PostfixExpr 'match' '{' CaseClauses '}'

  Expr1() : opt(Opt::UNDEFINED) {}
};

/**
 * Exprs ::= Expr {',' Expr}
 */
struct Exprs : ASTBase {
  spExpr expr;
  std::vector<std::pair<unsigned int, spExpr>> pairs;
};

/**
 * FunDef ::= FunSig [':' Type] '=' Expr
 *          | FunSig [nl] '{' Block '}'
 *          | 'this' ParamClause ParamClauses
 *            ('=' ConstrExpr | [nl] ConstrBlock)
 */
struct FunDef : ASTBase {
  enum class Opt {
    UNDEFINED,
    FUN_SIG_EQUALS_EXPR,
    FUN_SIG_BLOCK,
    THIS_PARAM_CLAUSE,
  };

  Opt opt;

  // FunSig [':' Type] '=' Expr
  spFunSig funSig;
  spTokenNode tokColon;
  spType type;
  spTokenNode tokEquals;
  spExpr expr;

  // TODO:
  // FunSig [nl] '{' Block '}'
  // 'this' ParamClause ParamClauses
  // ('=' ConstrExpr | [nl] ConstrBlock)

  FunDef() : opt(Opt::UNDEFINED) {}
};

/**
 * FunSig ::= id [FunTypeParamClause] ParamClauses
 */
struct FunSig : ASTBase {
  spLexId id;
  // TODO: [FunTypeParamClause]
  spParamClauses paramClauses;
};

/**
 * IdPeriod ::= id '.'
 */
struct IdPeriod : ASTBase {
  spTokenNode tok;
  spLexId id;
};

/**
 * Import ::= 'import' ImportExpr {',' ImportExpr}
 */
struct Import : ASTBase {
  spTokenNode tokImport;
  spImportExpr importExpr;
  std::vector<std::pair<spTokenNode, spImportExpr>> pairs;
};

/**
 * ImportExpr ::= QualId [ ('.' '_') | '.' ImportSelectors ]
 */
struct ImportExpr : ASTBase {
  spQualId qualId;
  spTokenNode tokPeriod;
  spTokenNode tokUnderscore;
  spImportSelectors importSelectors;
};

/**
 * ImportSelector ::= id ['=>' id | '=>' '_']
 */
struct ImportSelector : ASTBase {
  spLexId id;
  spTokenNode rArrow;
  spTokenNode tokUnderscore;
};

/**
 * ImportSelectors ::= '{' {ImportSelector ','} (ImportSelector | '_') '}'
 */
struct ImportSelectors : ASTBase {
  spTokenNode tokLCurlyB;
  spTokenNode tokRCurlyB;
  std::vector<std::pair<spImportSelector, spTokenNode>> pairs;
  spImportSelector importSelector;
  spTokenNode tokUnderscore;
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
 * InfixType ::= CompoundType {id [nl] CompoundType}
 */
struct InfixType : ASTBase {
  spCompoundType compoundType;
  // TODO: {id [nl] CompoundType}
};

/**
 * Literal ::= ['-'] integerLiteral
 *           | ['-'] floatingPointLiteral
 *           | booleanLiteral
 *           | characterLiteral
 *           | stringLiteral
 *           | symbolLiteral
 *           | 'null'
 */
struct Literal : ASTBase {
  enum class Opt {
    UNDEFINED,
    INTEGER,
    FLOATING_POINT,
    BOOLEAN,
    CHARACTER,
    STRING,
    SYMBOL,
    NULL_LITERAL,
  };

  Opt opt;

  spTokenNode tokMinus;

  // ['-'] integerLiteral
  spIntegerLiteral intLit;

  // ['-'] floatingPointLiteral
  spFloatingPointLiteral fpLit;

  // TODO: booleanLiteral
  // TODO: characterLiteral

  // stringLiteral
  spStringLiteral strLit;

  // TODO: symbolLiteral

  // 'null'
  spTokenNode tokNull;

  Literal() : opt(Opt::UNDEFINED) {}
};

/**
 * ObjectDef ::= id ClassTemplateOpt
 */
struct ObjectDef : ASTBase {
  spLexId id;
  spClassTemplateOpt classTmplOpt;
};

/**
 * Packaging ::= 'package' QualId [nl] '{' TopStatSeq '}'
 */
struct Packaging : ASTBase {
  spTokenNode tokPackage;
  spQualId qualId;
};

/**
 * Param ::= {Annotation} id [':' ParamType] ['=' Expr]
 */
struct Param : ASTBase {
  std::vector<spAnnotation> annotations;
  spLexId id;
  spTokenNode tokColon;
  spParamType paramType;
  spTokenNode tokEquals;
  spExpr expr;
};

/**
 * Params ::= Param {',' Param}
 */
struct Params : ASTBase {
  spParam param;
  std::vector<std::pair<spTokenNode, spParam>> pairs;
};

/**
 * ParamClause ::= [nl] '(' [Params] ')'
 */
struct ParamClause : ASTBase {
  spTokenNode tokLParen;
  spParams params;
  spTokenNode tokRParen;
};

/**
 * ParamClauses ::= {ParamClause} [[nl] '(' 'implicit' Params ')']
 */
struct ParamClauses : ASTBase {
  std::vector<spParamClause> paramClauses;

  spTokenNode tokLParen;
  spTokenNode tokImplicit;
  spParams params;
  spTokenNode tokRParen;
};

/**
 * ParamType ::= Type
 *             | '=>' Type
 *             | Type '*'
 */
struct ParamType : ASTBase {
  enum class Opt {
    UNDEFINED,
    TYPE,
    ARROW_TYPE,
    TYPE_STAR,
  };

  Opt opt;
  spType type;
  spTokenNode tokArrow;
  spTokenNode tokStar;

  ParamType() : opt(Opt::UNDEFINED) {}
};

/**
 * Path ::= StableId
 *        | [id '.'] 'this'
 */
struct Path : ASTBase {
  enum class Opt {
    UNDEFINED,
    STABLE_ID,
    THIS,
  };

  Opt opt;
  spStableId stableId;
  // TODO: [id '.'] 'this'

  Path() : opt(Opt::UNDEFINED) {}
};

/**
 * PrefixExpr ::= ['-' | '+' | '~' | '!'] SimpleExpr
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
 * QualId ::= id {'.' id}
 */
struct QualId : ASTBase {
  spLexId id;
  std::vector<spPeriodId> periodIds;
};

/**
 * semi ::= ';' | nl {nl}
 */
struct Semi : ASTBase {
  enum class Opt {
    UNDEFINED,
    SEMICOLON,
    NL,
  };

  Opt opt;
  spTokenNode tokSemiColon;

  Semi() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleExpr ::= 'new' (ClassTemplate | TemplateBody)
 *              | BlockExpr
 *              | SimpleExpr1 ['_']
 */
struct SimpleExpr : ASTBase {
  enum class Opt {
    UNDEFINED,
    NEW,
    BLOCK_EXPR,
    SIMPLE_EXPR1,
  };

  spTokenNode tokNew;
  spClassTemplate classTmpl;
  spTemplateBody tmplBody;
  spBlockExpr blockExpr;
  spSimpleExpr1 simpleExpr1;
  // TODO: ['_']

  Opt opt;

  SimpleExpr() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleExpr1 ::= SimpleExpr1Head SimpleExpr1Tail | SimpleExpr1Head
 */
struct SimpleExpr1 : ASTBase {
  spSimpleExpr1Head head;
  spSimpleExpr1Tail tail;
};

/**
 * SimpleExpr1Head ::= Literal
 *                   | Path
 *                   | '_'
 *                   | '(' [Exprs] ')'
 *                   | SimpleExpr '.' id
 *                   | SimpleExpr TypeArgs
 *                   | XmlExpr
 */
struct SimpleExpr1Head : ASTBase {
  enum class Opt {
    UNDEFINED,
    LITERAL,
    PATH,
    UNDERSCORE,
    EXPRS,
    SIMPLE_EXPR_ID,
    SIMPLE_EXPR_TYPE_ARGS,
    XMLEXPR,
  };

  Opt opt;

  // Literal
  spLiteral literal;

  // Path
  spPath path;

  // TODO: '_'
  // TODO: '(' [Exprs] ')'
  // TODO: SimpleExpr '.' id
  // TODO: SimpleExpr TypeArgs
  // TODO: XmlExpr

  SimpleExpr1Head() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleExpr1Tail ::=  ArgumentExprs SimpleExpr1Tail | ArgumentExprs
 */
struct SimpleExpr1Tail : ASTBase {
  spArgumentExprs argExprs;
  spSimpleExpr1Tail tail;
};

/**
 * SimpleType ::= SimpleTypeHead SimpleTypeTails | SimpleTypeHead
 */
struct SimpleType : ASTBase {
  spSimpleTypeHead head;
  spSimpleTypeTails tails;
};

/**
 * SimpleTypeHead ::= StableId
 *                  | Path '.' 'type'
 *                  | '(' Types ')'
 */
struct SimpleTypeHead : ASTBase {
  enum class Opt {
    UNDEFINED,
    STABLE_ID,
    PATH_TYPE,
    TYPES,
  };

  Opt opt;

  spStableId stableId;

  // TODO: Path '.' 'type'
  // TODO: '(' Types ')'

  SimpleTypeHead() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleTypeTail ::= TypeArgs | '#' id
 */
struct SimpleTypeTail : ASTBase {
  enum class Opt {
    UNDEFINED,
    TYPE_ARGS,
    HASH_ID,
  };

  Opt opt;

  // TODO: spTypeArgs
  // TODO: '#' id

  SimpleTypeTail() : opt(Opt::UNDEFINED) {}
};

/**
 * SimpleTypeTails ::= SimpleTypeTail SimpleTypeTails | SimpleTypeTail
 */
struct SimpleTypeTails : ASTBase {
  spSimpleTypeTail tail;
  spSimpleTypeTails tails;
};

/**
 * StableId ::= StableIdHead StableIdTail | StableIdHead
 */
struct StableId : ASTBase {
  spStableIdHead head;
  spStableIdTail tail;
};

/**
 * StableIdHead ::= id
 *                | [IdPeriod] 'this' PeriodId
 *                | [IdPeriod] 'super' [ClassQualifier] PeriodId
 */
struct StableIdHead : ASTBase {
  enum class Opt {
    UNDEFINED,
    ID,
    THIS,
    SUPER,
  };

  Opt opt;
  spLexId id;
  spIdPeriod idPeriod;
  spPeriodId periodId;
  spTokenNode tokThis;
  spTokenNode tokSuper;
  // TODO: ClassQualifier

  StableIdHead() : opt(Opt::UNDEFINED) {}
};

/**
 * StableIdTail ::= PeriodId StableIdTail | PeriodId
 */
struct StableIdTail : ASTBase {
  spPeriodId periodId;
  spStableIdTail tail;
};

/**
 * TemplateBody ::= [nl] '{' [SelfType] TemplateStat {semi TemplateStat} '}'
 */
struct TemplateBody : ASTBase {
  spTokenNode tokLCurlyB;
  // TODO:
  //spSelfType selfType;
  spTemplateStat tmplStat;
  // TODO:
  //std::vector<pair<spSemi, spTemplateStat>> paSemiTmplStat;
  spTokenNode tokRCurlyB;
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
  spDef def;
  // TODO: spExpr expr;

  TemplateStat() : opt(Opt::UNDEFINED) {}
};

/**
 * TmplDef ::= ['case'] 'class' ClassDef
 *           | ['case'] 'object' ObjectDef
 *           | 'trait' TraitDef
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
  spClassDef classDef;

  // OPT_CASE_OBJECT
  spTokenNode tokObject;
  spObjectDef objectDef;

  // OPT_TRAIT
  spTokenNode tokTrait;
  spTraitDef traitDef;

  TmplDef() : opt(Opt::UNDEFINED) {}
};

/**
 * TopStat ::= {Annotation [nl]} {Modifier} TmplDef
 *           | Import
 *           | Packaging
 *           | PackageObject
 */
struct TopStat : ASTBase {
  enum class Opt {
    UNDEFINED,
    TMPL_DEF,
    IMPORT,
    PACKAGING,
    PACKAGE_OBJECT,
  };

  Opt opt;

  std::vector<spAnnotation> annotations;

  // TODO:
  // std::vector<{Modifier}>
  spTmplDef tmplDef;

  spImport import;

  spPackaging packaging;

  // TODO: PackageObject

  TopStat() : opt(Opt::UNDEFINED) {}
};

/**
 * TopStatSeq ::= TopStat {semi TopStat}
 */
struct TopStatSeq : ASTBase {
  spTopStat topStat;
  std::vector<std::pair<spSemi, spTopStat>> pairs;
};

/**
 * TraitDef ::= id [TypeParamClause] TraitTemplateOpt
 */
struct TraitDef : ASTBase {
  spLexId id;
  // spTypeParamClause typeParamClause
  spTraitTemplateOpt traitTemplateOpt;
};

/**
 * TraitParents ::= AnnotType {'with' AnnotType}
 */
struct TraitParents : ASTBase {
  spAnnotType annotType;
  std::vector<std::pair<spTokenNode, spAnnotType>> pairs;
};

/**
 * TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody]
 */
struct TraitTemplate : ASTBase {
  // TODO: [EarlyDefs]
  spTraitParents traitParents;
  // TODO: [TemplateBody]
};

/**
 * TraitTemplateOpt ::= 'extends' TraitTemplate | [['extends'] TemplateBody]
 */
struct TraitTemplateOpt : ASTBase {
  enum class Opt {
    UNDEFINED,
    TRAIT_TEMPLATE,
    TEMPLATE_BODY,
  };

  Opt opt;

  spTokenNode tokExtends;
  spTraitTemplate traitTemplate;
  spTemplateBody templateBody;

  TraitTemplateOpt() : opt(Opt::UNDEFINED) {}
};

/**
 * Type ::= FunctionArgTypes '=>' Type
 *        | InfixType [ExistentialClause]
 */
struct Type : ASTBase {
  enum class Opt {
    UNDEFINED,
    FUNC_ARG_TYPES,
    INFIX_TYPE,
  };

  Opt opt;

  // TODO: FunctionArgTypes '=>' Type

  spInfixType infixType;
  // TODO: [ExistentialClause]

  Type() : opt(Opt::UNDEFINED) {}
};


} // namespace

#endif
