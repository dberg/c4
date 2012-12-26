//-*- C++ -*-
#ifndef __ANNOTATIONS_H__
#define __ANNOTATIONS_H__
#include <utility>
#include <vector>
#include <boost/shared_ptr.hpp>
#include "Token.h"
#include "ErrorCodes.h"

namespace djp {

typedef boost::shared_ptr<struct Annotation> spAnnotation;
typedef boost::shared_ptr<struct AnnotationElement> spAnnotationElement;
typedef boost::shared_ptr<struct Arguments> spArguments;
typedef boost::shared_ptr<struct ArrayCreatorRest> spArrayCreatorRest;
typedef boost::shared_ptr<struct ArrayCreatorRestOpt1> spArrayCreatorRestOpt1;
typedef boost::shared_ptr<struct ArrayCreatorRestOpt2> spArrayCreatorRestOpt2;
typedef boost::shared_ptr<struct ArrayInitializer> spArrayInitializer;
typedef boost::shared_ptr<struct AssignmentOperator> spAssignmentOperator;
typedef boost::shared_ptr<struct BasicType> spBasicType;
typedef boost::shared_ptr<struct Block> spBlock;
typedef boost::shared_ptr<struct BlockStatement> spBlockStatement;
typedef boost::shared_ptr<struct BooleanLiteral> spBooleanLiteral;
typedef boost::shared_ptr<struct Bound> spBound;
typedef boost::shared_ptr<struct Catches> spCatches;
typedef boost::shared_ptr<struct CatchClause> spCatchClause;
typedef boost::shared_ptr<struct CatchType> spCatchType;
typedef boost::shared_ptr<struct CharacterLiteral> spCharacterLiteral;
typedef boost::shared_ptr<struct ClassBody> spClassBody;
typedef boost::shared_ptr<struct ClassBodyDeclaration> spClassBodyDeclaration;
typedef boost::shared_ptr<struct ClassCreatorRest> spClassCreatorRest;
typedef boost::shared_ptr<struct ClassDeclaration> spClassDeclaration;
typedef boost::shared_ptr<struct ClassOrInterfaceDeclaration>
  spClassOrInterfaceDeclaration;
typedef boost::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef boost::shared_ptr<struct ConstructorDeclaratorRest>
  spConstructorDeclaratorRest;
typedef boost::shared_ptr<struct CreatedName> spCreatedName;
typedef boost::shared_ptr<struct Creator> spCreator;
typedef boost::shared_ptr<struct CreatorOpt1> spCreatorOpt1;
typedef boost::shared_ptr<struct CreatorOpt2> spCreatorOpt2;
typedef boost::shared_ptr<struct ElementValue> spElementValue;
typedef boost::shared_ptr<struct ElementValues> spElementValues;
typedef boost::shared_ptr<struct ElementValueArrayInitializer>
  spElementValueArrayInitializer;
typedef boost::shared_ptr<struct ElementValuePair> spElementValuePair;
typedef boost::shared_ptr<struct EnumDeclaration>
  spEnumDeclaration;
typedef boost::shared_ptr<struct ExplicitGenericInvocation>
  spExplicitGenericInvocation;
typedef boost::shared_ptr<struct ExplicitGenericInvocationSuffix>
  spExplicitGenericInvocationSuffix;
typedef boost::shared_ptr<struct Expression> spExpression;
typedef boost::shared_ptr<struct Expression1> spExpression1;
typedef boost::shared_ptr<struct Expression2> spExpression2;
typedef boost::shared_ptr<struct Expression3> spExpression3;
typedef boost::shared_ptr<struct Expression3Opt2> spExpression3Opt2;
typedef boost::shared_ptr<struct Expression3Opt3> spExpression3Opt3;
typedef boost::shared_ptr<struct Expression1Rest> spExpression1Rest;
typedef boost::shared_ptr<struct Expression2Rest> spExpression2Rest;
typedef boost::shared_ptr<struct ExpressionInBrackets> spExpressionInBrackets;
typedef boost::shared_ptr<struct FieldDeclaratorsRest> spFieldDeclaratorsRest;
typedef boost::shared_ptr<struct Finally> spFinally;
typedef boost::shared_ptr<struct FloatingPointLiteral> spFloatingPointLiteral;
typedef boost::shared_ptr<struct ForControl> spForControl;
typedef boost::shared_ptr<struct ForInit> spForInit;
typedef ForInit ForUpdate;
typedef boost::shared_ptr<struct ForInit> spForUpdate;
typedef boost::shared_ptr<struct ForVarControl> spForVarControl;
typedef boost::shared_ptr<struct ForVarControlRest> spForVarControlRest;
typedef boost::shared_ptr<struct ForVariableDeclaratorsRest>
  spForVariableDeclaratorsRest;
typedef boost::shared_ptr<struct FormalParameters> spFormalParameters;
typedef boost::shared_ptr<struct FormalParameterDecls> spFormalParameterDecls;
typedef boost::shared_ptr<struct FormalParameterDeclsRest>
  spFormalParameterDeclsRest;
typedef boost::shared_ptr<struct Identifier> spIdentifier;
typedef boost::shared_ptr<struct IdentifierSuffix> spIdentifierSuffix;
typedef boost::shared_ptr<struct ImportDeclaration> spImportDeclaration;
typedef boost::shared_ptr<struct ImportDeclarations> spImportDeclarations;
typedef boost::shared_ptr<struct IntegerLiteral> spIntegerLiteral;
typedef boost::shared_ptr<struct InterfaceDeclaration> spInterfaceDeclaration;
typedef boost::shared_ptr<struct InnerCreator> spInnerCreator;
typedef boost::shared_ptr<struct Literal> spLiteral;
typedef boost::shared_ptr<struct LocalVariableDeclarationStatement>
  spLocalVariableDeclarationStatement;
typedef boost::shared_ptr<struct MemberDecl> spMemberDecl;
typedef boost::shared_ptr<struct MethodDeclaratorRest> spMethodDeclaratorRest;
typedef boost::shared_ptr<struct MethodOrFieldDecl> spMethodOrFieldDecl;
typedef boost::shared_ptr<struct MethodOrFieldRest> spMethodOrFieldRest;
typedef boost::shared_ptr<struct Modifier> spModifier;
typedef boost::shared_ptr<struct NormalClassDeclaration>
  spNormalClassDeclaration;
typedef boost::shared_ptr<struct NonWildcardTypeArguments>
  spNonWildcardTypeArguments;
typedef boost::shared_ptr<struct NonWildcardTypeArgumentsOrDiamond>
  spNonWildcardTypeArgumentsOrDiamond;
typedef boost::shared_ptr<struct PackageDeclaration> spPackageDeclaration;
typedef boost::shared_ptr<struct ParExpression> spParExpression;
typedef boost::shared_ptr<struct PostfixOp> spPostfixOp;
typedef boost::shared_ptr<struct PrefixOp> spPrefixOp;
typedef boost::shared_ptr<struct Primary> spPrimary;
typedef boost::shared_ptr<struct PrimaryBasicType> spPrimaryBasicType;
typedef boost::shared_ptr<struct PrimaryIdentifier> spPrimaryIdentifier;
typedef boost::shared_ptr<struct PrimarySuperSuperSuffix>
  spPrimarySuperSuperSuffix;
typedef boost::shared_ptr<struct PrimaryThisArguments> spPrimaryThisArguments;
typedef boost::shared_ptr<struct PrimaryNewCreator> spPrimaryNewCreator;
typedef boost::shared_ptr<struct PrimaryNonWildcardTypeArguments>
  spPrimaryNonWildcardTypeArguments;
typedef boost::shared_ptr<struct PrimaryVoidClass> spPrimaryVoidClass;
typedef boost::shared_ptr<struct QualifiedIdentifier> spQualifiedIdentifier;
typedef boost::shared_ptr<struct QualifiedIdentifierList>
  spQualifiedIdentifierList;
typedef boost::shared_ptr<struct ReferenceType> spReferenceType;
typedef boost::shared_ptr<struct ReferenceTypeTriplet> spReferenceTypeTriplet;
typedef boost::shared_ptr<struct Selector> spSelector;
typedef boost::shared_ptr<struct Statement> spStatement;
typedef boost::shared_ptr<struct StatementExpression> spStatementExpression;
typedef boost::shared_ptr<struct StringLiteral> spStringLiteral;
typedef boost::shared_ptr<struct SuperSuffix> spSuperSuffix;
typedef boost::shared_ptr<struct TokenExp> spTokenExp;
typedef boost::shared_ptr<struct TokenExp> spNullLiteral;
typedef boost::shared_ptr<struct Type> spType;
typedef boost::shared_ptr<struct TypeArgument> spTypeArgument;
typedef boost::shared_ptr<struct TypeArguments> spTypeArguments;
typedef boost::shared_ptr<struct TypeArgumentOpt2> spTypeArgumentOpt2;
typedef boost::shared_ptr<struct TypeArgumentsOrDiamond>
  spTypeArgumentsOrDiamond;
typedef boost::shared_ptr<struct TypeDeclaration> spTypeDeclaration;
typedef boost::shared_ptr<struct TypeList> spTypeList;
typedef boost::shared_ptr<struct TypeParameter> spTypeParameter;
typedef boost::shared_ptr<struct TypeParameters> spTypeParameters;
typedef boost::shared_ptr<struct VariableDeclarator> spVariableDeclarator;
typedef boost::shared_ptr<struct VariableDeclarators> spVariableDeclarators;
typedef boost::shared_ptr<struct VariableDeclaratorId> spVariableDeclaratorId;
typedef boost::shared_ptr<struct VariableDeclaratorRest>
  spVariableDeclaratorRest;
typedef boost::shared_ptr<struct VariableInitializer> spVariableInitializer;
typedef boost::shared_ptr<struct VariableModifier> spVariableModifier;
typedef boost::shared_ptr<struct VoidMethodDeclaratorRest>
  spVoidMethodDeclaratorRest;

typedef std::pair<unsigned int, unsigned int> ArrayPair;
typedef std::vector<ArrayPair> ArrayDepth;
typedef boost::shared_ptr<struct Comment> spComment;
typedef std::pair<unsigned int, unsigned int> Diamond;
typedef struct TokenExp NullLiteral;

struct ASTError {
  bool err;
  int errIdx;
  ASTError() : err(false), errIdx(0) {}
  void addErr(int _errIdx) { err = true; errIdx = _errIdx; }
};

struct Comment {
  enum CommentOpt {
    OPT_UNDEFINED,
    OPT_ONE_LINE,
    OPT_MULTIPLE_LINES,
  };

  CommentOpt opt;
  unsigned int posIni;
  unsigned int posEnd;

  Comment() : opt(OPT_UNDEFINED), posIni(0), posEnd(0) {}
};

/// CompilationUnit:
///   [PackageDeclaration]
///   {ImportDeclarations}
///   {TypeDeclarations}
struct CompilationUnit {
  spPackageDeclaration pkgDecl;
  spImportDeclarations impDecls;
  std::vector<spTypeDeclaration> typeDecls;
};

/// PackageDeclaration: [ [Annotations]  package QualifiedIdentifier ; ]
struct PackageDeclaration {
  std::vector<spAnnotation> annotations;
  int pkgTokPos;
  spQualifiedIdentifier qualifiedId;
  bool err;
  PackageDeclaration() : pkgTokPos(-1), err(false) {}
};

/// TypeDeclaration: ClassOrInterfaceDeclaration ;
struct TypeDeclaration {
  spClassOrInterfaceDeclaration decl;
};

/// ClassOrInterfaceDeclaration:
///   {Modifier} (ClassDeclaration | InterfaceDeclaration)
struct ClassOrInterfaceDeclaration : ASTError {
  spModifier modifier;
  spClassDeclaration classDecl;
  spInterfaceDeclaration interfaceDecl;
};

struct TokenExp {
  int pos;
  int type;

  TokenExp(int pos = -1, int type = -1) : pos(pos), type(type) {}
};

/// Modifier:
///   Annotations
///   public
///   protected
///   private
///   static
///   abstract
///   final
///   native
///   synchronized
///   transient
///   volatile
///   strictfp
struct Modifier {
  std::vector<spAnnotation> annotations;
  std::vector<spTokenExp> tokens;
};

/// PrefixOp: ++ -- ! ~ + -
struct PrefixOp {
  int pos;
  int token;

  PrefixOp(unsigned int pos, int token) : pos(pos), token(token) {}
};

/// ClassDeclaration: NormalClassDeclaration | EnumDeclaration
struct ClassDeclaration {
  spNormalClassDeclaration nClassDecl;
  spEnumDeclaration enumDecl;
};

/// NormalClassDeclaration:
///   class Identifier [TypeParameters] [extends Type] [implements TypeList]
///     ClassBody
/// Detailed reference:
/// ClassModifiers(opt) class Identifier TypeParameters(opt)
///                           Super(opt) Interfaces(opt) ClassBody
struct NormalClassDeclaration : ASTError {
  spTokenExp classTok;
  spIdentifier identifier;
  spTypeParameters typeParams;
  spTokenExp extendsTok;
  spType type;
  spTokenExp implementsTok;
  spTypeList typeList;
  spClassBody classBody;
};

/// ClassBody: '{' {ClassBodyDeclaration} '}'
struct ClassBody : ASTError {
  unsigned posLCBrace;
  unsigned posRCBrace;
  std::vector<spClassBodyDeclaration> decls;

  ClassBody() : posLCBrace(0), posRCBrace(0) {}
};

/// ClassBodyDeclaration:
///   ;
///   {Modifier} MemberDecl
///   [static] Block
/// Our Modifier structure defines an array of annotation and tokens so we treat
/// {Modifier} as spModifier.
///
/// For further reference.
/// ClassBodyDeclaration:
///   ClassMemberDeclaration
///   InstanceInitializer
///   StaticInitializer
///   ConstructorDeclaration
struct ClassBodyDeclaration {
  enum ClassBodyDeclarationOpt {
    OPT_UNDEFINED,
    OPT_MODIFIER_MEMBER_DECL,  // { Modifier } MemberDecl
    OPT_STATIC_BLOCK,          // [static] Block
  };

  ClassBodyDeclarationOpt opt;

  // OPT_MODIFIER_MEMBER_DECL
  spModifier modifier;
  spMemberDecl memberDecl;

  // TODO:
  // OPT_STATIC_BLOCK
  //spTokenExp staticToken;
  //spBlock block;

  ClassBodyDeclaration() : opt(OPT_UNDEFINED) {}
};

/// MemberDecl:
///   (1) MethodOrFieldDecl
///   (2) void Identifier VoidMethodDeclaratorRest
///   (3) Identifier ConstructorDeclaratorRest
///   (4) GenericMethodOrConstructorDecl
///   (5) ClassDeclaration
///   (6) InterfaceDeclaration
struct MemberDecl : ASTError {
  enum MemberDeclOpt {
    OPT_UNDEFINED,
    OPT_METHOD_OR_FIELD_DECL,
    OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST,
    OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST,
    OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL,
    OPT_CLASS_DECLARATION,
    OPT_INTERFACE_DECLARATION,
  };

  MemberDeclOpt opt;

  // Shared by 2,3
  spIdentifier id;

  // (1) MethodOrFieldDecl
  spMethodOrFieldDecl methodOrFieldDecl;

  // (2) void Identifier VoidMethodDeclaratorRest
  spTokenExp tokVoid;
  spVoidMethodDeclaratorRest voidMethDeclRest;

  // (3) Identifier ConstructorDeclaratorRest
  spConstructorDeclaratorRest constDeclRest;

  // TODO:
  // GenericMethodOrConstructorDecl

  // TODO:
  // ClassDeclaration

  // TODO:
  // InterfaceDeclaration

  MemberDecl() : opt(OPT_UNDEFINED) {}
};

/// MethodDeclaratorRest:
///  FormalParameters {'[' ']'} [throws QualifiedIdentifierList] (Block | ;)
struct MethodDeclaratorRest : ASTError {
  spFormalParameters formParams;
  ArrayDepth arrayDepth;

  spTokenExp tokThrows;
  spQualifiedIdentifierList qualifiedIdList;

  spBlock block;
  unsigned posSemiColon;

  MethodDeclaratorRest() : posSemiColon(0) {}
};

/// MethodOrFieldDecl: Type Identifier MethodOrFieldRest
struct MethodOrFieldDecl : ASTError {
  spType type;
  spIdentifier id;
  spMethodOrFieldRest methodOrFieldRest;
};

/// MethodOrFieldRest:
///   (1) FieldDeclaratorsRest ;
///   (2) MethodDeclaratorRest
struct MethodOrFieldRest : ASTError {
  enum MethodOrFieldRestOpt {
    OPT_UNDEFINED,
    OPT_FIELD,
    OPT_METHOD,
  };

  MethodOrFieldRestOpt opt;

  // (1) FieldDeclaratorsRest ;
  spFieldDeclaratorsRest fieldDeclsRest;
  unsigned posSemiColon;

  // (2) MethodDeclaratorRest
  spMethodDeclaratorRest methodDeclRest;

  MethodOrFieldRest() : opt(OPT_UNDEFINED) {}
};

/// ConstructorDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] Block
struct ConstructorDeclaratorRest : ASTError {
  spFormalParameters formParams;
  spTokenExp tokThrows;
  spQualifiedIdentifierList qualifiedIdList;
  spBlock block;
};

/// FormalParameters: '(' [FormalParameterDecls] ')'
struct FormalParameters : ASTError {
  unsigned int posLParen;
  unsigned int posRParen;
  spFormalParameterDecls formParamDecls;
  FormalParameters() : posLParen(0), posRParen(0) {}
};

/// FormalParameterDecls: {VariableModifier} Type FormalParameterDeclsRest
struct FormalParameterDecls {
  spVariableModifier varModifier;
  spType type;
  spFormalParameterDeclsRest formParamDeclsRest;
};

/// VariableModifier:
///   final
///   Annotation
/// One 'final' keyword is allowed, while we can have zero or more annotations.
struct VariableModifier : ASTError {
  spTokenExp tokFinal;
  std::vector<spAnnotation> annotations;

  bool isEmpty() {
    if (!tokFinal && annotations.size() == 0) {
      return true;
    }

    return false;
  }
};

/// VoidMethodDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] (Block | ;)
struct VoidMethodDeclaratorRest : ASTError {
  spFormalParameters formParams;

  spTokenExp tokThrows;
  spQualifiedIdentifierList qualifiedIdList;

  spBlock block;
  unsigned posSemiColon;

  VoidMethodDeclaratorRest() : posSemiColon(0) {}
};

/// Type:
///   BasicType {[]}
///   ReferenceType {[]}
struct Type : ASTError {
  enum TypeOpt {
    OPT_UNDEFINED,
    OPT_BASIC_TYPE,
    OPT_REFERENCE_TYPE,
  };

  TypeOpt opt;
  spBasicType basicType;
  spReferenceType refType;
  ArrayDepth arrayDepth;

  Type() : opt(OPT_UNDEFINED) {}
  bool isEmpty() { return opt == OPT_UNDEFINED; }
};

/// FormalParameterDeclsRest:
///   VariableDeclaratorId [ , FormalParameterDecls ]
///   ... VariableDeclaratorId
/// The variable arity parameter must be the last parameter.
struct FormalParameterDeclsRest {
  enum FormalParameterDeclsRestOpt {
    OPT_UNDEFINED,
    OPT_VAR_DECL_ID,
    OPT_VAR_ARITY,
  };

  FormalParameterDeclsRestOpt opt;
  spVariableDeclaratorId varDeclId;
  spFormalParameterDecls formParamDecls;

  FormalParameterDeclsRest() : opt(OPT_UNDEFINED) {}
};

/// VariableDeclarator: Identifier VariableDeclaratorRest
struct VariableDeclarator : ASTError {
  spIdentifier id;
  spVariableDeclaratorRest varDeclRest;
};

/// VariableDeclarators: VariableDeclarator { , VariableDeclarator }
struct VariableDeclarators : ASTError {
  spVariableDeclarator varDecl;
  std::vector<std::pair<unsigned, spVariableDeclarator> > semiColonAndVarDecls;
};

/// VariableDeclaratorId: Identifier {[]}
struct VariableDeclaratorId {
  spIdentifier identifier;
  ArrayDepth arrayDepth;
};

/// VariableDeclaratorRest: {'[' ']'} [ = VariableInitializer ]
struct VariableDeclaratorRest : ASTError {
  ArrayDepth arrayDepth;
  unsigned int posEquals;
  spVariableInitializer varInit;
};

/// BasicType: byte | short | char | int | long | float | double | boolean
struct BasicType {
  spTokenExp token;
  BasicType(spTokenExp token) : token(token) {}
};

/// ReferenceType:
///   Identifier [TypeArguments] { . Identifier [TypeArguments] }
struct ReferenceType : ASTError {
  spIdentifier id;
  spTypeArguments typeArgs;
  // { . Identifier [TypeArguments] }
  std::vector<spReferenceTypeTriplet> triplets;
};

/// ReferenceType helper
///   { . Identifier [TypeArguments] }
struct ReferenceTypeTriplet {
  unsigned posPeriod;
  spIdentifier id;
  spTypeArguments typeArgs;

  ReferenceTypeTriplet() : posPeriod(0) {}
};

/// Selector:
///   . Identifier [Arguments]
///   . ExplicitGenericInvocation
///   . this
///   . super SuperSuffix
///   . new [NonWildcardTypeArguments] InnerCreator
///   '[' Expression ']'
struct Selector : ASTError {
  enum SelectorOpt {
    OPT_UNDEFINED,
    OPT_IDENTIFIER_ARGUMENTS,
    OPT_EXPLICIT_GENERIC_INVOCATION,
    OPT_THIS,
    OPT_SUPER_SUPER_SUFFIX,
    OPT_NEW,
    OPT_EXPRESSION,
  };

  SelectorOpt opt;

  // shared 1-5
  unsigned int posPeriod;

  // opt1: . Identifier [Arguments]
  spIdentifier id;
  spArguments args;

  // opt2: . ExplicitGenericInvocation
  spExplicitGenericInvocation explGenInvocation;

  // opt3: . this
  spTokenExp tokThis;

  // opt4: . super SuperSuffix
  spTokenExp tokSuper;
  spSuperSuffix superSuffix;

  // opt5: . new [NonWildcardTypeArguments] InnerCreator
  spTokenExp tokNew;
  spNonWildcardTypeArguments nonWildcardTypeArguments;
  spInnerCreator innerCreator;

  // opt6: '[' Expression ']'
  ArrayPair arrayPair;
  spExpression expr;

  Selector() : opt(OPT_UNDEFINED) {}
};

/// Statement:
///   (1) Block
///   (2) ;
///   (3) Identifier : Statement
///   (4) StatementExpression ;
///   (5) if ParExpression Statement [else Statement]
///   (6) assert Expression [: Expression] ;
///   (7) switch ParExpression '{' SwitchBlockStatementGroups '}'
///   (8) while ParExpression Statement
///   (9) do Statement while ParExpression ;
///   (10) for '(' ForControl ')' Statement
///   (11) break [Identifier] ;
///   (12) continue [Identifier] ;
///   (13) return [Expression] ;
///   (14) throw Expression ;
///   (15) synchronized ParExpression Block
///   (16) try Block ( Catches | [Catches] Finally )
///   (17) try ResourceSpecification Block [Catches] [Finally]
struct Statement : ASTError {
  enum StatementOpt {
    OPT_UNDEFINED,
    OPT_BLOCK,
    OPT_SEMI_COLON,
    OPT_ID_STMT,
    OPT_STMT_EXPR,
    OPT_IF,
    OPT_ASSERT,
    OPT_SWITCH,
    OPT_WHILE,
    OPT_DO,
    OPT_FOR,
    OPT_BREAK,
    OPT_CONTINUE,
    OPT_RETURN,
    OPT_THROW,
    OPT_SYNC,
    OPT_TRY_BLOCK,
    OPT_TRY_RESOURCE,
  };

  StatementOpt opt;

  // shared 1,15,16
  spBlock block;

  // shared 2,4,12,13,14
  unsigned posSemiColon;

  // shared 5,8,15
  spParExpression parExpr;

  // shared 3,12
  spIdentifier id;

  // (1) Block

  // (2) ';'

  // (3) Identifier : Statement
  unsigned int posColon;
  spStatement stmt;

  // (4) StatementExpression ;
  spStatementExpression stmtExpr;

  // (5) if ParExpression Statement [else Statement]
  spTokenExp tokIf;
  spStatement stmtIf;
  spTokenExp tokElse;
  spStatement stmtElse;

  // (8) while ParExpression Statement
  spTokenExp tokWhile;
  spStatement stmtWhile;

  // (10) for '(' ForControl ')' Statement
  spTokenExp tokFor;
  unsigned posLParen;
  unsigned posRParen;
  spForControl forCtrl;
  spStatement stmtFor;

  // (12) continue [Identifier] ;
  spTokenExp tokContinue;

  // (13) return [Expression] ;
  spTokenExp tokReturn;
  spExpression exprReturn;

  // (14) throw Expression ;
  spTokenExp tokThrow;
  spExpression throwExpr;

  // (15) synchronized ParExpression Block
  spTokenExp tokSync;

  // (16) try Block ( Catches | [Catches] Finally )
  spTokenExp tokTry;
  spCatches catches;
  spFinally finally;

  Statement()
    : opt(OPT_UNDEFINED), posSemiColon(0), posLParen(0), posRParen(0) {}
};

/// StatementExpression: Expression
struct StatementExpression : ASTError {
  spExpression expr;
};

/// TypeArguments: < TypeArgument { , TypeArgument } >
struct TypeArguments : ASTError {
  unsigned int posLt;
  unsigned int posGt;
  spTypeArgument typeArg;
  std::vector<spTypeArgument> typeArgs;

  TypeArguments() : ASTError(), posLt(0), posGt(0) {}
};

/// TypeArgument:
///   ReferenceType
///   ? [(extends|super) ReferenceType]
struct TypeArgument : ASTError {
  enum TypeArgumentOpt {
    OPT_UNDEFINED,
    OPT_REFERENCE_TYPE,
    OPT_QUESTION_MARK,
  };

  TypeArgumentOpt opt;
  spReferenceType refType; // opt1
  spTypeArgumentOpt2 opt2;

  TypeArgument() : opt(OPT_UNDEFINED) {}
};

/// TypeArgument: ? [(extends|super) ReferenceType]
struct TypeArgumentOpt2 : ASTError {
  unsigned int posQuestionMark;
  spTokenExp tokExtendsOrSuper;
  spReferenceType refType;
};

/// Block: '{' BlockStatements '}'
struct Block : ASTError {
  unsigned posLCBracket;
  unsigned posRCBracket;
  std::vector<spBlockStatement> blockStmts;

  Block() : posLCBracket(0), posRCBracket(0) {}
};

/// BlockStatement:
///   (1) LocalVariableDeclarationStatement
///   (2) ClassOrInterfaceDeclaration
///   (3) [Identifier :] Statement
struct BlockStatement : ASTError {
  enum BlockStatementOpt {
    OPT_UNDEFINED,
    OPT_LOCAL_VAR,
    OPT_CLASS_OR_INTERFACE_DECL,
    OPT_ID_STMT,
  };

  BlockStatementOpt opt;

  // (1) LocalVariableDeclarationStatement
  spLocalVariableDeclarationStatement localVar;

  // (2) ClassOrInterfaceDeclaration
  spClassOrInterfaceDeclaration decl;

  // (3) [Identifier :] Statement
  spIdentifier id;
  unsigned int posColon;
  spStatement stmt;

  BlockStatement() : opt(OPT_UNDEFINED) {}
};

/// TODO:
struct EnumDeclaration {

};

/// InterfaceDeclaration:
///   NormalInterfaceDeclaration
///   AnnotationTypeDeclaration
// TODO:
struct InterfaceDeclaration {

};

/// ImportDeclarations:
///   ImportDeclaration
///   ImportDeclarations ImportDeclaration
struct ImportDeclarations {
  std::vector<spImportDeclaration> imports;
  ImportDeclarations(std::vector<spImportDeclaration> imports)
    : imports(imports) {}
};

/// InnerCreator:
///   Identifier [NonWildcardTypeArgumentsOrDiamond] ClassCreatorRest
struct InnerCreator : ASTError {
  spIdentifier id;
  spNonWildcardTypeArgumentsOrDiamond nonWildcardOrDiam;
  spClassCreatorRest classCreatorRest;
};

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
struct Annotation {
  int posTokAt;
  bool err;
  spQualifiedIdentifier qualifiedId;
  spAnnotationElement elem;
  Annotation() : posTokAt(-1), err(false) {}
};

/// Identifier: IdentifierChars but not Keyword or BooleanLiteral or NullLiteral
/// IdentifierChars: JavaLetter | IdentifierChars JavaLetterOrDigit
/// JavaLetter: any Unicode character that is a Java letter
/// JavaLetterOrDigit: any Unicde character that is a Java letter or digit
struct Identifier {
  int pos;
  const std::string value;
  Identifier(int pos, const std::string &value) : pos(pos), value(value) {}
};

/// IdentifierSuffix:
///   '[' ( {'[' ']'} . class | Expression ) ']'
///   Arguments
///   . ( class | ExplicitGenericInvocation | this | super Arguments |
///       new [NonWildcardTypeArguments] InnerCreator )
struct IdentifierSuffix : ASTError {
  enum IdentifierSuffixOpt {
    OPT_UNDEFINED,
    OPT_ARRAY_ARRAY_DEPTH_CLASS,
    OPT_ARRAY_EXPRESSION,
    OPT_ARGUMENTS,
    OPT_PERIOD_CLASS,
    OPT_PERIOD_EXPLICIT_GENERIC_INVOCATION,
    OPT_PERIOD_THIS,
    OPT_PERIOD_SUPER_ARGUMENTS,
    OPT_NEW,
  };

  IdentifierSuffixOpt opt;

  // shared 1,2
  ArrayPair arrayPair;

  // shared 2,4-8
  int posPeriod;

  // shared 2,4
  spTokenExp tokClass;

  // shared 3,7
  spArguments args;

  // opt1: '[' {'[' ']'} . class ']'
  ArrayDepth arrayDepth;

  // opt2: '[' Expression ']'
  spExpression expr;

  // opt3: Arguments

  // opt4: . class

  // opt5: . ExplicitGenericInvocation
  spExplicitGenericInvocation explGenInvocation;

  // opt6: . this
  spTokenExp tokThis;

  // opt7: . super Arguments
  spTokenExp tokSuper;

  // opt8: . new [NonWildcardTypeArguments] InnerCreator
  spTokenExp tokNew;
  spNonWildcardTypeArguments nonWildcardTypeArguments;
  spInnerCreator innerCreator;

  IdentifierSuffix() : opt(OPT_UNDEFINED), posPeriod(0) {}
};

/// QualifiedIdentifier: Identifier { . Identifier }
struct QualifiedIdentifier : ASTError {
  spIdentifier id;
  std::vector<std::pair<unsigned, spIdentifier> > pairs;
};

/// QualifiedIdentifierList:
///   QualifiedIdentifier { , QualifiedIdentifier }
struct QualifiedIdentifierList : ASTError {
  spQualifiedIdentifier qualifiedId;
  std::vector<std::pair<unsigned, spQualifiedIdentifier> > pairs;
};

/// For simplicity we adopt the following rule
/// ImportDeclaration: import [static] QualifiedId [.*]
/// For reference, a more detailed ImportDeclaration follows:
/// ImportDeclaration
///   SingleTypeImportDeclaration
///   TypeImportOnDemandDeclaration
///   SingleStaticImportDeclaration
///   StaticImportOnDemandDeclaration
struct ImportDeclaration {
  int posTokImport;
  int posTokStatic;

  // [.*]
  unsigned iniOnDemand;
  unsigned endOnDemand;

  bool err;
  spQualifiedIdentifier qualifiedId;
  ImportType type;

  ImportDeclaration()
    : posTokImport(-1), posTokStatic(-1), iniOnDemand(0), endOnDemand(0),
      err(false), type(SINGLE_TYPE_IMPORT_DECLARATION) {}
};

/// AnnotationElement: ElementValuePairs | ElementValue
struct AnnotationElement {
  enum AnnotationElementOpt {
    OPT_UNDEFINED,
    OPT_ELEMENT_VALUE_PAIRS,
    OPT_ELEMENT_VALUE,
  };

  AnnotationElementOpt opt;
  bool err;
  std::vector<spElementValuePair> pairs;
  spElementValue value;

  AnnotationElement() : opt(OPT_UNDEFINED), err(false) {}
};

/// ElementValuePairs: ElementValuePair {, ElementValuePair }
/// ElementValuePair: Identifier = ElementValue
struct ElementValuePair {
  spIdentifier id;
  spElementValue value;
  ElementValuePair() {}
};

/// ElementValue: Annotation | Expression1 | ElementValueArrayInitializer
struct ElementValue : ASTError {
  enum ElementValueOpt {
    OPT_UNDEFINED,
    OPT_ANNOTATION,
    OPT_EXPRESSION1,
    OPT_ELEMENT_VALUE_ARRAY_INITIALIZER,
  };

  ElementValueOpt opt;
  spAnnotation annotation;
  spExpression1 expr1;
  spElementValueArrayInitializer elemValArrayInit;

  ElementValue() : opt(OPT_UNDEFINED) {}
};

/// ElementValues:
///   ElementValue { , ElementValue }
struct ElementValues : ASTError {
  spElementValue elemVal;
  std::vector<std::pair<unsigned, spElementValue> > pairs;
};

/// ElementValueArrayInitializer: '{' [ElementValues] [,] '}'
struct ElementValueArrayInitializer : ASTError {
  unsigned posLCBrace;
  unsigned posRCBrace;
  unsigned posComma;
  spElementValues elemVals;

  ElementValueArrayInitializer() : posLCBrace(0), posRCBrace(0), posComma(0) {}
};

/// ExplicitGenericInvocation:
///   NonWildcardTypeArguments ExplicitGenericInvocationSuffix
struct ExplicitGenericInvocation : ASTError {
  spNonWildcardTypeArguments nonWildcardTypeArguments;
  spExplicitGenericInvocationSuffix explGen;
};

/// ExplicitGenericInvocationSuffix:
///   super SuperSuffix
///   Identifier Arguments
struct ExplicitGenericInvocationSuffix : ASTError {
  enum ExplicitGenericInvocationSuffixOpt {
    OPT_UNDEFINED,
    OPT_SUPER,
    OPT_IDENTIFIER,
  };

  ExplicitGenericInvocationSuffixOpt opt;

  // opt1
  spTokenExp tokSuper;
  spSuperSuffix superSuffix;

  // opt2
  spIdentifier id;
  spArguments args;

  ExplicitGenericInvocationSuffix() : opt(OPT_UNDEFINED) {}
};

/// Expression: Expression1 [ AssignmentOperator Expression1 ]
struct Expression {
  spExpression1 expr1;
  // [ AssignmentOperator Expression1 ]
  spAssignmentOperator assignOp;
  spExpression1 assignExpr1;

  bool isEmpty() {
    return (expr1) ? false : true;
  }
};

/// Expression1: Expression2 [Expression1Rest]
struct Expression1 {
  spExpression2 expr2;
  spExpression1Rest expr1Rest;

  bool isEmpty() {
    return (expr2) ? false : true;
  }
};

/// Expression1Rest: ? Expression : Expression1
struct Expression1Rest : ASTError {
  unsigned posQuestionMark;
  unsigned posColon;
  spExpression expr;
  spExpression1 expr1;

  Expression1Rest() : posQuestionMark(0), posColon(0) {}
};

/// Expression2: Expression3 [ Expression2Rest ]
struct Expression2 {
  spExpression3 expr3;
  spExpression2Rest expr2Rest;

  bool isEmpty() {
    return (expr3) ? false : true;
  }
};

/// Expression2Rest:
///   (1) { InfixOp Expression3 }
///   (2) instanceof Type
struct Expression2Rest : ASTError {
  enum Expression2RestOpt {
    OPT_UNDEFINED,
    OPT_INFIXOP_EXPR3,
    OPT_INSTANCEOF_TYPE,
  };

  Expression2RestOpt opt;

  // (1)
  std::vector<std::pair<spTokenExp, spExpression3> > pairs;

  // (2)
  spTokenExp tokInstanceOf;
  spType type;

  Expression2Rest() : opt(OPT_UNDEFINED) {}
};

/// Arguments: '(' [ Expression { , Expression }] ')'
struct Arguments : ASTError {
  unsigned int posLParen;
  unsigned int posRParen;
  spExpression expr;
  std::vector<std::pair<unsigned int, spExpression> > exprs;

  Arguments() : posLParen(0), posRParen(0) {}
};

/// Expression3:
///   (1) PrefixOp Expression3
///   (2) '(' Type ')' Expression3
///   (3) '(' Expression ')' Expression3
///   (3) Primary { Selector } { PostfixOp }
/// The italicized parenthesis are probably a typo in the grammar.
/// We treat them as terminals.
struct Expression3 : ASTError {
  enum Expression3Opt {
    OPT_UNDEFINED,
    OPT_PREFIXOP_EXPRESSION3,
    OPT_TYPE_EXPRESSION3,
    OPT_EXPRESSION_EXPRESSION3,
    OPT_PRIMARY_SELECTOR_POSTFIXOP,
  };

  Expression3Opt opt;

  // (1) PrefixOp Expression3
  spPrefixOp prefixOp;
  spExpression3 expr3;

  // 2,3
  spExpression3Opt2 opt2;
  spExpression3Opt3 opt3;

  // (3) Primary { Selector } { PostfixOp }
  spPrimary primary;
  std::vector<spSelector> selectors;
  std::vector<spPostfixOp> postfixOps;

  Expression3() : opt(OPT_UNDEFINED) {}
  bool isEmpty() { return opt == OPT_UNDEFINED; }
};

/// Expression3:
/// (2) '(' Type ')' Expression3
struct Expression3Opt2 : ASTError {
  unsigned posLParen;
  unsigned posRParen;
  spType type;
  spExpression3 expr3;

  Expression3Opt2() : posLParen(0), posRParen(0) {}
};

/// Expression3:
///   (3) '(' Expression ')' Expression3
struct Expression3Opt3 : ASTError {
  unsigned posLParen;
  unsigned posRParen;
  spExpression expr;
  spExpression3 expr3;

  Expression3Opt3() : posLParen(0), posRParen(0) {}
};

/// Primary: 
///   (1) Literal
///   (2) ParExpression
///   (3) this [Arguments]
///   (4) super SuperSuffix
///   (5) new Creator
///   (6) NonWildcardTypeArguments
///         ( ExplicitGenericInvocationSuffix | this Arguments )
///   (7) Identifier { . Identifier } [IdentifierSuffix]
///   (8) BasicType {[]} . class
///   (9) void . class
struct Primary : ASTError {
  enum PrimaryEnum {
    OPT_UNDEFINED,
    OPT_LITERAL,
    OPT_PAR_EXPRESSION,
    OPT_THIS_ARGUMENTS,
    OPT_SUPER_SUPER_SUFFIX,
    OPT_NEW_CREATOR,
    OPT_NON_WILDCARD_TYPE_ARGUMENTS,
    OPT_IDENTIFIER,
    OPT_BASIC_TYPE,
    OPT_VOID_CLASS,
  };

  PrimaryEnum opt;
  spLiteral literal;
  spParExpression parExpr;
  spPrimaryThisArguments thisArgs;
  spPrimarySuperSuperSuffix superSuperSuffix;
  spPrimaryNewCreator newCreator;
  spPrimaryNonWildcardTypeArguments nonWildcardTypeArguments;
  spPrimaryIdentifier primaryId;
  spPrimaryVoidClass primaryVoidClass;
  spPrimaryBasicType primaryBasicType;

  Primary() : opt(OPT_UNDEFINED) {}
  bool isEmpty() { return opt == OPT_UNDEFINED; }
};

/// Primary: BasicType {[]} . class
struct PrimaryBasicType : ASTError {
  spBasicType basicType;
  ArrayDepth arrayDepth;
  unsigned int posComma;
  spTokenExp tokClass;
};

/// Primary: Identifier { . Identifier } [IdentifierSuffix]
struct PrimaryIdentifier : ASTError {
  std::vector<spIdentifier> ids;
  spIdentifierSuffix idSuffix;
};

/// Primary: this [Arguments]
struct PrimaryThisArguments {
  spTokenExp tokThis;
  spArguments args;
};

/// Primary: super SuperSuffix
struct PrimarySuperSuperSuffix {
  spTokenExp tokSuper;
  spSuperSuffix superSuffix;
};

/// Primary: new Creator
struct PrimaryNewCreator {
  spTokenExp tokNew;
  spCreator creator;
};

/// Primary:
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
struct PrimaryNonWildcardTypeArguments : ASTError {
  enum PrimaryNonWildcardTypeArgumentsOpt {
    OPT_UNDEFINED,
    OPT_EXPLICIT_GENERIC_INVOCATION_SUFFIX,
    OPT_THIS_ARGUMENTS,
  };

  PrimaryNonWildcardTypeArgumentsOpt opt;
  spNonWildcardTypeArguments nonWildcardTypeArguments;

  // opt1
  spExplicitGenericInvocationSuffix explGen;

  // opt2
  spTokenExp tokThis;
  spArguments args;

  PrimaryNonWildcardTypeArguments() : opt(OPT_UNDEFINED) {};
};

/// Primary: void . class
struct PrimaryVoidClass : ASTError {
  spTokenExp tokVoid;
  unsigned int posComma;
  spTokenExp tokClass;

  PrimaryVoidClass() : posComma(0) {}
};

/// Literal:
///   IntegerLiteral
///   FloatingPointLiteral
///   CharacterLiteral
///   StringLiteral
///   BooleanLiteral
///   NullLiteral
struct Literal {
  enum LiteralEnum {
    OPT_UNDEFINED,
    OPT_INTEGER,
    OPT_FLOATING_POINT,
    OPT_CHAR,
    OPT_STRING,
    OPT_BOOLEAN,
    OPT_NULL,
  };

  LiteralEnum opt;
  spIntegerLiteral intLiteral;
  spFloatingPointLiteral fpLiteral;
  spCharacterLiteral charLiteral;
  spStringLiteral strLiteral;
  spBooleanLiteral boolLiteral;
  spNullLiteral nullLiteral;

  Literal() : opt(OPT_UNDEFINED) {}

  bool isEmpty() { return opt == OPT_UNDEFINED; }
};

/// LocalVariableDeclarationStatement:
///   { VariableModifier } Type VariableDeclarators ;
struct LocalVariableDeclarationStatement : ASTError{
  spVariableModifier varModifier;
  spType type;
  spVariableDeclarators varDecls;
  unsigned posSemiColon;

  LocalVariableDeclarationStatement() : posSemiColon(0) {}
};

/// IntegerLiteral:
///   DecimalIntegerLiteral
///   HexIntegerLiteral
///   OctalIntegerLiteral
///   BinaryIntegerLiteral
struct IntegerLiteral {
  enum IntegerLiteralEnum {
    OPT_UNDEFINED,
    OPT_DECIMAL,
    OPT_HEX,
    OPT_OCTAL,
    OPT_BINARY,
  };

  IntegerLiteralEnum opt;
  bool intSuffix;
  int pos;
  std::string value;

  IntegerLiteral() : opt(OPT_UNDEFINED), intSuffix(false), pos(-1) {}
};

/// FloatingPointLiteral
///   DecimalFloatingPointLiteral
///   HexadecimalFloatingPointLiteral
struct FloatingPointLiteral {
  enum FloatingPointLiteralEnum {
    OPT_UNDEFINED,
    OPT_DECIMAL,
    OPT_HEX,
  };

  FloatingPointLiteralEnum opt;
  int pos;
  std::string value;

  FloatingPointLiteral() : opt(OPT_UNDEFINED), pos(-1) {}
};

/// ForControl
///   (1) ForVarControl
///   (2) ForInit ; [Expression] ; [ForUpdate]
struct ForControl : ASTError {
  enum ForControlOpt {
    OPT_UNDEFINED,
    OPT_FOR_VAR_CTRL,
    OPT_FOR_INIT,
  };

  ForControlOpt opt;

  // (1) ForVarControl
  spForVarControl varCtrl;

  // (2) ForInit ; [Expression] ; [ForUpdate]
  spForInit forInit;
  unsigned posSemiColon1;
  unsigned posSemiColon2;
  spExpression expr;
  spForUpdate forUpdate;

  ForControl() : opt(OPT_UNDEFINED), posSemiColon1(0), posSemiColon2(0) {}
};

/// ForInit:
/// ForUpdate:
///   StatementExpression { , StatementExpression }
struct ForInit : ASTError {
  spStatementExpression stmtExpr;
  std::vector<std::pair<unsigned, spStatementExpression> > pairs;
};

/// ForVarControl
///   {VariableModifier} Type VariableDeclaratorId ForVarControlRest
struct ForVarControl : ASTError {
  spVariableModifier varMod;
  spType type;
  spVariableDeclaratorId varDeclId;
  spForVarControlRest forVarCtrlRest;
};

/// ForVarControlRest
///   (1) ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
///   (2) : Expression
struct ForVarControlRest : ASTError {
  enum ForVarControlRestOpt {
    OPT_UNDEFINED,
    OPT_FOR_VAR_DECLS_REST,
    OPT_COLON_EXPR,
  };

  ForVarControlRestOpt opt;

  // Shared 1,2
  spExpression expr;

  // (1) ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
  spForVariableDeclaratorsRest forVarDeclsRest;
  unsigned posSemiColon1;
  unsigned posSemiColon2;
  spForUpdate forUpdate;

  // (2) : Expression
  unsigned posColon;

  ForVarControlRest()
    : opt(OPT_UNDEFINED), posSemiColon1(0), posSemiColon2(0), posColon(0) {}
};

/// ForVariableDeclaratorsRest
///   [ = VariableInitializer ] { , VariableDeclarator }
struct ForVariableDeclaratorsRest : ASTError {
  unsigned posEquals;
  spVariableInitializer varInit;
  std::vector<std::pair<unsigned, spVariableDeclarator> > pairs;

  ForVariableDeclaratorsRest() : posEquals(0) {}
};

struct BooleanLiteral {
  int pos;
  bool val;
};

/// Bound:
///   ReferenceType { & ReferenceType }
struct Bound : ASTError {
  spReferenceType refType;
  std::vector<std::pair<unsigned, spReferenceType> > pairs;
};

/// Catches: CatchClause { CatchClause }
struct Catches : ASTError {
  spCatchClause catchClause;
  std::vector<spCatchClause> catchClauses;
};

/// CatchClause:
///   catch '(' {VariableModifier} CatchType Identifier ')' Block
struct CatchClause : ASTError {
  unsigned posLParen;
  unsigned posRParen;
  spTokenExp tokCatch;
  std::vector<spVariableModifier> varMods;
  spCatchType catchType;
  spIdentifier id;
  spBlock block;

  CatchClause() : posLParen(0), posRParen(0) {}
};

/// CatchType: Identifier { '|' Identifier }
struct CatchType : ASTError {
  spIdentifier id;
  std::vector<std::pair<unsigned, spIdentifier> > pipeAndId;
};

struct CharacterLiteral {
  int pos;
  std::string val;
};

struct StringLiteral {
  int pos;
  std::string val;
};

/// ParExpression: '(' Expression ')'
struct ParExpression : ASTError {
  unsigned posLParen;
  unsigned posRParen;
  spExpression expr;

  ParExpression() : posLParen(0), posRParen(0) {}
};

/// PostfixOp: ++ | --
struct PostfixOp {
  enum PostfixOpOpt {
    OPT_UNDEFINED,
    OPT_PLUS_PLUS,
    OPT_MINUS_MINUS,
  };

  PostfixOpOpt opt;
  unsigned int pos;

  PostfixOp() : opt(OPT_UNDEFINED), pos(0) {}
};

/// SuperSuffix:
///   Arguments
///   . Identifier [Arguments]
struct SuperSuffix : ASTError {
  enum SuperSuffixEnum {
    OPT_UNDEFINED,
    OPT_ARGUMENTS,
    OPT_IDENTIFIER_ARGUMENTS,
  };

  SuperSuffixEnum opt;
  unsigned posPeriod;
  spIdentifier id;
  spArguments args;

  SuperSuffix() : opt(OPT_UNDEFINED), posPeriod(0) {}
};

/// Creator:
///   NonWildcardTypeArguments CreatedName ClassCreatorRest
///   CreatedName ( ClassCreatorRest | ArrayCreatorRest )
struct Creator : ASTError {
  enum CreatorEnum {
    OPT_UNDEFINED,
    OPT_NON_WILDCARD_TYPE_ARGUMENTS,
    OPT_CREATED_NAME,
  };

  CreatorEnum opt;
  spCreatorOpt1 opt1;
  spCreatorOpt2 opt2;

  Creator() : opt(OPT_UNDEFINED) {}
};

/// CreatorOpt1: NonWildcardTypeArguments CreatedName ClassCreatorRest
struct CreatorOpt1 : ASTError {
  spNonWildcardTypeArguments nonWildcardTypeArguments;
  spCreatedName createdName;
  spClassCreatorRest classCreatorRest;
};

/// CreatorOpt2: CreatedName ( ClassCreatorRest | ArrayCreatorRest )
struct CreatorOpt2 : ASTError {
  spCreatedName createdName;
  spClassCreatorRest classCreatorRest;
  spArrayCreatorRest arrayCreatorRest;
};

/// NonWildcardTypeArguments: < TypeList >
struct NonWildcardTypeArguments : ASTError {
  unsigned int posLt;
  unsigned int posGt;
  spTypeList typeList;
  NonWildcardTypeArguments() : posLt(0), posGt(0) {}
};

/// NonWildcardTypeArgumentsOrDiamond:
///   < >
///   NonWildcardTypeArguments
struct NonWildcardTypeArgumentsOrDiamond : ASTError {
  enum NonWildcardTypeArgumentsOrDiamondOpt {
    OPT_UNDEFINED,
    OPT_DIAMOND,
    OPT_NON_WILDCARD_TYPE_ARGUMENTS,
  };

  NonWildcardTypeArgumentsOrDiamondOpt opt;
  Diamond diamond; // opt1
  spNonWildcardTypeArguments nonWildcardTypeArguments; // opt2

  NonWildcardTypeArgumentsOrDiamond() : opt(OPT_UNDEFINED) {}
};

/// TypeList: ReferenceType {, ReferenceType }
struct TypeList : ASTError {
  spReferenceType refType;
  std::vector<spReferenceType> refTypes;
};

/// TypeParameter:
///   Identifier [extends Bound]
struct TypeParameter : ASTError {
  spIdentifier id;
  spTokenExp tokExtends;
  spBound bound;
};

/// TypeParameters:
///   < TypeParameter { , TypeParameter } >
struct TypeParameters : ASTError {
  unsigned posLt;
  unsigned posGt;
  spTypeParameter typeParam;
  std::vector<std::pair<unsigned, spTypeParameter> > pairs;

  TypeParameters() : posLt(0), posGt(0) {}
};

/// CreatedName:
///   Identifier [TypeArgumentsOrDiamond]
///     { . Identifier [TypeArgumentsOrDiamond] }
struct CreatedName : ASTError {
  spIdentifier id;
  spTypeArgumentsOrDiamond typeArgsOrDiam;
  std::vector<spCreatedName> createdNames;
};

/// TypeArgumentsOrDiamond:
///   < >
///   TypeArguments
struct TypeArgumentsOrDiamond : ASTError {
  enum TypeArgumentsOrDiamondOpt {
    OPT_UNDEFINED,
    OPT_DIAMOND,
    OPT_TYPE_ARGUMENTS,
  };

  TypeArgumentsOrDiamondOpt opt;

  // opt 1
  unsigned int posLt;
  unsigned int posGt;

  // opt 2
  spTypeArguments typeArgs;

  TypeArgumentsOrDiamond() : ASTError(), opt(OPT_UNDEFINED), posLt(0), posGt(0) {}
};

/// ClassCreatorRest: Arguments [ClassBody]
struct ClassCreatorRest : ASTError {
  spArguments args;
  spClassBody classBody;
};

/// ArrayCreatorRest:
///   '['
///     ( ']' { '[]' } ArrayInitializer |
///       Expression ']' { '[' Expression ']' } { '[]' } )
///   ']'  <--- probably a typo in the grammar.
///
/// Non-terminals are enclosed in square brackets.
struct ArrayCreatorRest : ASTError {
  enum ArrayCreatorRestOpt {
    OPT_UNDEFINED,
    OPT_ARRAY_INITIALIZER,
    OPT_EXPRESSION,
  };

  ArrayCreatorRestOpt opt;
  spArrayCreatorRestOpt1 opt1;
  spArrayCreatorRestOpt2 opt2;

  ArrayCreatorRest() : opt(OPT_UNDEFINED) {}
};

/// ArrayCreatorRestOpt1:
///   '[' ']' { '[]' } ArrayInitializer
struct ArrayCreatorRestOpt1 : ASTError {
  ArrayDepth arrayDepth;
  spArrayInitializer arrayInitializer;
};

/// ArrayCreatorRestOpt2:
///   '[' Expression ']' { '[' Expression ']' } { '[]' }
struct ArrayCreatorRestOpt2 : ASTError {
  spExpressionInBrackets exprInBrackets;
  std::vector<spExpressionInBrackets> exprInBracketsList;
  ArrayDepth arrayDepth;
};

/// Helper structure
/// ExpressionInBrackets: '[' Expression ']'
struct ExpressionInBrackets : ASTError {
  unsigned posLBracket;
  unsigned posRBracket;
  spExpression expr;
  ExpressionInBrackets() : posLBracket(0), posRBracket(0) {}
};

/// FieldDeclaratorsRest: VariableDeclaratorRest { , VariableDeclarator }
struct FieldDeclaratorsRest : ASTError {
  spVariableDeclaratorRest varDeclRest;
  std::vector<std::pair<unsigned int, spVariableDeclarator> > pairsCommaVarDecl;
};

/// Finally: finally Block
struct Finally : ASTError {
  spTokenExp tokFinally;
  spBlock block;
};

/// ArrayInitializer:
///   '{' [ VariableInitializer { , VariableInitializer } [,] ] '}'
struct ArrayInitializer : ASTError {
  unsigned posLCBrace;
  unsigned posRCBrace;
  unsigned posComma;
  spVariableInitializer varInit;
  // { , VariableInitializer }
  std::vector<std::pair<unsigned, spVariableInitializer> > pairs;

  ArrayInitializer() : posLCBrace(0), posRCBrace(0), posComma(0) {}
};

/// AssignmentOperator:
///   =
///   +=
///   -=
///   *=
///   /=
///   &=
///   |=
///   ^=
///   %=
///   <<=
///   >>=
///   >>>=
struct AssignmentOperator : ASTError {
  spTokenExp tok;
};

/// VariableInitializer:
///   ArrayInitializer
///   Expression
struct VariableInitializer :ASTError {
  enum VariableInitializerOpt {
    OPT_UNDEFINED,
    OPT_ARRAY_INITIALIZER,
    OPT_EXPRESSION,
  };

  VariableInitializerOpt opt;
  spArrayInitializer arrayInit;
  spExpression expr;
};

} // namespace
#endif
