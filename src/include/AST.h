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
typedef boost::shared_ptr<struct BasicType> spBasicType;
typedef boost::shared_ptr<struct Block> spBlock;
typedef boost::shared_ptr<struct BlockStatement> spBlockStatement;
typedef boost::shared_ptr<struct BooleanLiteral> spBooleanLiteral;
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
typedef boost::shared_ptr<struct Expression1Rest> spExpression1Rest;
typedef boost::shared_ptr<struct Expression2Rest> spExpression2Rest;
typedef boost::shared_ptr<struct ExpressionInBrackets> spExpressionInBrackets;
typedef boost::shared_ptr<struct FieldDeclaratorsRest> spFieldDeclaratorsRest;
typedef boost::shared_ptr<struct FloatingPointLiteral> spFloatingPointLiteral;
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
typedef boost::shared_ptr<struct MemberDecl> spMemberDecl;
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
typedef boost::shared_ptr<struct PairExpression> spPairExpression;
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
typedef boost::shared_ptr<struct ReferenceType> spReferenceType;
typedef boost::shared_ptr<struct Selector> spSelector;
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
typedef boost::shared_ptr<struct VariableDeclarator> spVariableDeclarator;
typedef boost::shared_ptr<struct VariableDeclaratorId> spVariableDeclaratorId;
typedef boost::shared_ptr<struct VariableDeclaratorRest>
  spVariableDeclaratorRest;
typedef boost::shared_ptr<struct VariableInitializer> spVariableInitializer;
typedef boost::shared_ptr<struct VariableModifier> spVariableModifier;

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
struct ClassOrInterfaceDeclaration {
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
  //std::vector<spTypeParameters>
  spTokenExp extendsTok;
  spType type;
  //spTokenExp implements
  //spTypeList
  spClassBody classBody;
};

/// ClassBody: '{' {ClassBodyDeclaration} '}'
struct ClassBody : ASTError {
  std::vector<spClassBodyDeclaration> decls;
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
struct MemberDecl {
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

  // (1) MethodOrFieldDecl
  spMethodOrFieldDecl methodOrFieldDecl;

  // TODO:
  // void Identifier VoidMethodDeclaratorRest

  // Identifier ConstructorDeclaratorRest
  spIdentifier identifier;
  spConstructorDeclaratorRest constDeclRest;

  // TODO:
  // GenericMethodOrConstructorDecl

  // TODO:
  // ClassDeclaration

  // TODO:
  // InterfaceDeclaration

  MemberDecl() : opt(OPT_UNDEFINED) {}
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
  unsigned int posSemiColon;

  // TODO:
  // (2) MethodDeclaratorRest

  MethodOrFieldRest() : opt(OPT_UNDEFINED) {}
};

/// ConstructorDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] Block
struct ConstructorDeclaratorRest {
  spFormalParameters formParams;
  // spIdentifier throws;
  // spQualifiedIdentifierList
  spBlock block;
};

/// FormalParameters: ( [FormalParameterDecls] )
struct FormalParameters {
  int error;
  spFormalParameterDecls formParamDecls;
  FormalParameters() : error(0) {}
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
struct VariableModifier {
  spTokenExp tokFinal;
  std::vector<spAnnotation> annotations;

  bool isEmpty() {
    if (!tokFinal && annotations.size() == 0) {
      return true;
    }

    return false;
  }
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

/// Identifier VariableDeclaratorRest
struct VariableDeclarator {
  spIdentifier id;
  spVariableDeclaratorRest varDeclRest;
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
  std::vector<spReferenceType> refTypes;
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
  unsigned int posComma;

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
struct Block {
  std::vector<spBlockStatement> blockStmts;
};

/// TODO:
struct BlockStatement {

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
struct QualifiedIdentifier {
  std::vector<spIdentifier> identifiers;
  int ini, end;
  QualifiedIdentifier(std::vector<spIdentifier> identifiers)
    : identifiers(identifiers) {
    ini = (identifiers[0])->pos;
    spIdentifier last = identifiers[identifiers.size() - 1];
    end = last->pos + last->value.length() - 1;
  }

  // We currently use this for test purposes only
  std::string getQualifiedIdentifier() {
    std::string qualifiedId = "";
    int last_key = identifiers.size() - 1;
    for (int i = 0; i <= last_key ; i++) {
      qualifiedId += identifiers[i]->value;
      if (i != last_key) {
        qualifiedId += ".";
      }
    }

    return qualifiedId;
  }
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
  int iniOnDemand;
  int endOnDemand;

  bool err;
  spQualifiedIdentifier qualifiedId;
  ImportType type;

  ImportDeclaration()
    : posTokImport(-1), posTokStatic(-1), iniOnDemand(-1), endOnDemand(-1),
      err(false), type(SINGLE_TYPE_IMPORT_DECLARATION) {}

  // We currently use this for test purposes only
  std::string getImport() {
    std::string import = qualifiedId->getQualifiedIdentifier();
    if (iniOnDemand > 0) {
      import += ".*";
    }
    return import;
  }
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
struct ElementValue {
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

/// ElementValueArrayInitializer: { [ElementValues] [,] }
struct ElementValueArrayInitializer {
  std::vector<spElementValue> elemValues;
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
  // TODO: [ AssignmentOperator Expression1 ]

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
struct Expression1Rest {
  // TODO:
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
///   { InfixOp Expression3 }
///   instanceof Type
struct Expression2Rest {
  // TODO:
};

/// Arguments: '(' [ Expression { , Expression }] ')'
struct Arguments : ASTError {
  unsigned int posLParen;
  unsigned int posRParen;
  std::vector<spExpression> exprs;

  Arguments() : posLParen(0), posRParen(0) {}
};

/// Expression3:
///   (1) PrefixOp Expression3
///   (2) ( Expression | Type ) Expression3
///   (3) Primary { Selector } { PostfixOp }
struct Expression3 : ASTError {
  enum Expression3Opt {
    OPT_UNDEFINED,
    OPT_PREFIXOP_EXPRESSION3,
    OPT_EXPRESSION_TYPE_EXPRESSION3,
    OPT_PRIMARY_SELECTOR_POSTFIXOP,
  };

  Expression3Opt opt;

  // (1) PrefixOp Expression3
  spPrefixOp prefixOp;
  spExpression3 expr3;

  // (2) ( Expression | Type ) Expression3
  spExpression expr;
  spType type;

  // (3) Primary { Selector } { PostfixOp }
  spPrimary primary;
  spSelector selector;
  spPostfixOp postfixOp;

  Expression3() : opt(OPT_UNDEFINED) {}
  bool isEmpty() { return opt == OPT_UNDEFINED; }
};

/// Primary: 
///   Literal
///   ParExpression
///   this [Arguments]
///   super SuperSuffix
///   new Creator
///   NonWildcardTypeArguments
///     ( ExplicitGenericInvocationSuffix | this Arguments )
///   Identifier { . Identifier } [IdentifierSuffix]
///   BasicType {[]} . class
///   void . class
struct Primary {
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
  spPairExpression pairExpr;
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

struct BooleanLiteral {
  int pos;
  bool val;
};

struct CharacterLiteral {
  int pos;
  std::string val;
};

struct StringLiteral {
  int pos;
  std::string val;
};

struct PairExpression {
  spExpression expr;
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
struct SuperSuffix {
  enum SuperSuffixEnum {
    OPT_UNDEFINED,
    OPT_ARGUMENTS,
    OPT_IDENTIFIER_ARGUMENTS,
  };

  SuperSuffixEnum opt;
  int err;
  spIdentifier id;
  spArguments args;

  SuperSuffix() : opt(OPT_UNDEFINED), err(0) {}
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
///   ']'  <--- probably a typo.
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
  std::vector<spExpressionInBrackets> exprInBracketsList;
  ArrayDepth arrayDepth;
};

/// Helper structure
/// ExpressionInBrackets: '[' Expression ']'
struct ExpressionInBrackets : ASTError {
  unsigned int posOpenBracket;
  unsigned int posCloseBracket;
  spExpression expr;
  ExpressionInBrackets() : posOpenBracket(0), posCloseBracket(0) {}
};

/// FieldDeclaratorsRest: VariableDeclaratorRest { , VariableDeclarator }
struct FieldDeclaratorsRest : ASTError {
  spVariableDeclaratorRest varDeclRest;
  std::vector<std::pair<unsigned int, spVariableDeclarator> > pairsCommaVarDecl;
};

/// ArrayInitializer:
///   '{' [ VariableInitializer { , VariableInitializer } [,] ] '}'
struct ArrayInitializer : ASTError {
  unsigned int posOpenCBrace;
  unsigned int posCloseCBrace;
  std::vector<spVariableInitializer> varInitList;

  ArrayInitializer() : posOpenCBrace(0), posCloseCBrace(0) {}
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
