//-*- C++ -*-
#ifndef __SCALA_AST_H__
#define __SCALA_AST_H__
#include <memory>
#include <vector>
#include <utility>
#include "ScalaToken.h"

/**
 * We have a few modifications in the grammar.
 *
 * SimpleExpr1 was transformed to remove the left recursion into the following
 * production rules:
 *
 * SimpleExpr1 ::= SimpleExpr1Head SimpleExpr1Tail | SimpleExpr1Head
 *
 * SimpleExpr1Head ::= Literal
 *                   | Path
 *                   | ‘_’
 *                   | ‘(’ [Exprs] ‘)’
 *                   | SimpleExpr ‘.’ id
 *                   | SimpleExpr TypeArgs
 *                   | XmlExpr
 *
 * SimpleExpr1Tail ::=  ArgumentExprs SimpleExpr1Tail | ArgumentExprs
 */

namespace djp {
namespace scala {

typedef struct ASTBase LexId;
typedef std::shared_ptr<LexId> spLexId;
typedef struct ASTBase StringLiteral;
typedef std::shared_ptr<StringLiteral> spStringLiteral;

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
typedef std::shared_ptr<struct Expr> spExpr;
typedef std::shared_ptr<struct Expr1> spExpr1;
typedef std::shared_ptr<struct Exprs> spExprs;
typedef std::shared_ptr<struct InfixExpr> spInfixExpr;
typedef std::shared_ptr<struct Literal> spLiteral;
typedef std::shared_ptr<struct ObjectDef> spObjectDef;
typedef std::shared_ptr<struct Path> spPath;
typedef std::shared_ptr<struct PrefixExpr> spPrefixExpr;
typedef std::shared_ptr<struct PostfixExpr> spPostfixExpr;
typedef std::shared_ptr<struct Semi> spSemi;
typedef std::shared_ptr<struct SimpleExpr> spSimpleExpr;
typedef std::shared_ptr<struct SimpleExpr1> spSimpleExpr1;
typedef std::shared_ptr<struct SimpleExpr1Head> spSimpleExpr1Head;
typedef std::shared_ptr<struct SimpleExpr1Tail> spSimpleExpr1Tail;
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
  spTokenNode tokLParen;
  spTokenNode tokRParen;
  spTokenNode tokComma;
  spExprs exprs;
  spPostfixExpr postfixExpr;
  // TODO: ‘:’ ‘_’ ‘*’
  // TODO: [nl]
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
 * Expr ::= (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr
 *        | Expr1
 */
struct Expr : ASTBase {
  enum class Opt {
    UNDEFINED,
    EXPR,
    EXPR1,
  };

  Opt opt;
  // TODO: (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr
  spExpr1 expr1;

  Expr() : opt(Opt::UNDEFINED) {}
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
 * Exprs ::= Expr {‘,’ Expr}
 */
struct Exprs : ASTBase {
  spExpr expr;
  std::vector<std::pair<unsigned int, spExpr>> pairs;
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
 * Literal ::= [‘-’] integerLiteral
 *           | [‘-’] floatingPointLiteral
 *           | booleanLiteral
 *           | characterLiteral
 *           | stringLiteral
 *           | symbolLiteral
 *           | ‘null’
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

  // TODO: [‘-’] integerLiteral
  // TODO: [‘-’] floatingPointLiteral
  // TODO: booleanLiteral
  // TODO: characterLiteral

  // stringLiteral
  spStringLiteral strLit;

  // TODO: symbolLiteral

  // ‘null’
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
    SEMICOLON,
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
 * SimpleExpr1 ::= SimpleExpr1Head SimpleExpr1Tail | SimpleExpr1Head
 */
struct SimpleExpr1 : ASTBase {
  spSimpleExpr1Head head;
  spSimpleExpr1Tail tail;
};

/**
 * SimpleExpr1Head ::= Literal
 *                   | Path
 *                   | ‘_’
 *                   | ‘(’ [Exprs] ‘)’
 *                   | SimpleExpr ‘.’ id
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

  // TODO: ‘_’
  // TODO: ‘(’ [Exprs] ‘)’
  // TODO: SimpleExpr ‘.’ id
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
    TMPL_DEF,
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
