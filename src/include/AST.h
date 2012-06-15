//-*- C++ -*-
#ifndef __ANNOTATIONS_H__
#define __ANNOTATIONS_H__
#include <vector>
#include <boost/shared_ptr.hpp>
#include "Token.h"
#include "ErrorCodes.h"

namespace djp {

typedef boost::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef boost::shared_ptr<struct PackageDeclaration> spPackageDeclaration;
typedef boost::shared_ptr<struct ImportDeclarations> spImportDeclarations;
typedef boost::shared_ptr<struct ImportDeclaration> spImportDeclaration;
typedef boost::shared_ptr<struct Annotation> spAnnotation;
typedef boost::shared_ptr<struct QualifiedIdentifier> spQualifiedIdentifier;
typedef boost::shared_ptr<struct AnnotationElement> spAnnotationElement;
typedef boost::shared_ptr<struct ElementValue> spElementValue;
typedef boost::shared_ptr<struct ElementValuePair> spElementValuePair;
typedef boost::shared_ptr<struct Identifier> spIdentifier;
typedef boost::shared_ptr<struct Expression1> spExpression1;
typedef boost::shared_ptr<struct Expression2> spExpression2;
typedef boost::shared_ptr<struct Expression3> spExpression3;
typedef boost::shared_ptr<struct Primary> spPrimary;
typedef boost::shared_ptr<struct Literal> spLiteral;
typedef boost::shared_ptr<struct IntegerLiteral> spIntegerLiteral;
typedef boost::shared_ptr<struct DecimalIntegerLiteral> spDecimalIntegerLiteral;
typedef boost::shared_ptr<struct DecimalNumeral> spDecimalNumeral;
typedef boost::shared_ptr<struct TypeDeclarations> spTypeDeclarations;
typedef boost::shared_ptr<struct TypeDeclaration> spTypeDeclaration;
typedef boost::shared_ptr<struct ClassOrInterfaceDeclaration>
  spClassOrInterfaceDeclaration;
typedef boost::shared_ptr<struct ClassDeclaration> spClassDeclaration;
typedef boost::shared_ptr<struct NormalClassDeclaration>
  spNormalClassDeclaration;
typedef boost::shared_ptr<struct ClassBody> spClassBody;
typedef boost::shared_ptr<struct ClassBodyDeclaration> spClassBodyDeclaration;
typedef boost::shared_ptr<struct ConstructorDeclaratorRest> spConstructorDeclaratorRest;
typedef boost::shared_ptr<struct FormalParameterDecl> spFormalParameterDecl;
typedef boost::shared_ptr<struct Block> spBlock;
typedef boost::shared_ptr<struct BlockStatement> spBlockStatement;
typedef boost::shared_ptr<struct EnumDeclaration>
  spEnumDeclaration;
typedef boost::shared_ptr<struct InterfaceDeclaration> spInterfaceDeclaration;
typedef boost::shared_ptr<struct MemberDecl> spMemberDecl;
typedef boost::shared_ptr<struct Modifier> spModifier;
typedef boost::shared_ptr<struct TokenExp> spTokenExp;
typedef boost::shared_ptr<struct Error> spError;
typedef boost::shared_ptr<struct Warning> spWarning;

/// CompilationUnit:
///   [PackageDeclaration]
///   {ImportDeclarations}
///   {TypeDeclarations}
struct CompilationUnit {
  spPackageDeclaration pkgDecl;
  spImportDeclarations impDecls;
  std::vector<spTypeDeclaration> typeDecls;
  std::vector<spError> errors;
  std::vector<spWarning> warnings;
  CompilationUnit() {}
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
struct NormalClassDeclaration {
  spTokenExp classTok;
  spIdentifier identifier;
  //std::vector<spTypeParameters>
  //spTokenExp extends;
  //spType
  //spTokenExp implements
  //spTypeList
  spClassBody classBody;
};

/// ClassBody: '{' {ClassBodyDeclaration} '}'
struct ClassBody {
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

  spModifier modifier;
  spMemberDecl memberDecl;

  // TODO:
  //spTokenExp staticToken;
  //spBlock block;

  ClassBodyDeclaration() : opt(OPT_UNDEFINED) {}
};

/// MemberDecl:
///   MethodOrFieldDecl
///   void Identifier VoidMethodDeclaratorRest
///   Identifier ConstructorDeclaratorRest
///   GenericMethodOrConstructorDecl
///   ClassDeclaration
///   InterfaceDeclaration
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

  // TODO:
  // MethodOrFieldDecl

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

/// ConstructorDeclaratorRest:
///   FormalParameters [throws QualifiedIdentifierList] Block
struct ConstructorDeclaratorRest {
  std::vector<spFormalParameterDecl> formParamDecls;
  // spIdentifier throws;
  // spQualifiedIdentifierList
  spBlock block;
};

/// FormalParameters: ( [FormalParameterDecls] )
struct FormalParameterDecl {

};

/// FormalParameterDecls: {VariableModifier} Type FormalParameterDeclsRest

/// Block: { BlockStatements }
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

/// ElementValuePair: ElementValuePairs | ElementValue
/// TODO: they should be a union
struct AnnotationElement {
  bool err;
  std::vector<spElementValuePair> pairs;
  spElementValue value;
  AnnotationElement() : err(false) {}
};

/// ElementValuePair: Identifier = ElementValue
struct ElementValuePair {
  spIdentifier id;
  spElementValue value;
  ElementValuePair() {}
};

/// TODO: Enable the commented structures as union
struct ElementValue {
  //Annotation annotation;
  spExpression1 expr1;
  //ElementValueArrayInitializer elemValArrayInit;
  ElementValue() {}
};

/// Expression1: Expression2 [Expression1Rest]
/// TODO: Expression1Rest
struct Expression1 {
  spExpression2 expr2;
  // [ Expression1Rest ]
  Expression1() {}
};

/// Expression2: Expression3 [ Expression2Rest ]
/// TODO: Expression2Rest
struct Expression2 {
  spExpression3 expr3;
  // [ Expression2Rest ]
  Expression2() {}
};

/// Expression3:
///   PrefixOp Expression3
///   ( Expression | Type ) Expression3
///   Primary { Selector } { PostfixOp }
/// TODO: deal with 1st and 3rd case using union
struct Expression3 {
  spPrimary primary; // TODO: { Selector } { PostfixOp }
  Expression3() {}
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
/// TODO: We're only dealing with Literal now
struct Primary {
  spLiteral literal;
  Primary() {}
};

/// Literal:
///   IntegerLiteral
///   FloatingPointLiteral
///   CharacterLiteral
///   StringLiteral
///   BooleanLiteral
///   NullLiteral
/// TODO: We're only dealing with IntegerLiteral now
struct Literal {
  spIntegerLiteral intLiteral;
  Literal() {}
};

/// IntegerLiteral:
///   DecimalIntegerLiteral
///   HexIntegerLiteral
///   OctalIntegerLiteral
///   BinaryIntegerLiteral
/// TODO: We're only dealing with DecimalIntegerLiteral now
struct IntegerLiteral {
  spDecimalIntegerLiteral decIntLiteral;
  IntegerLiteral() {}
};

/// DecimalIntegerLiteral: DecimalNumeral [IntegerTypeSuffix]
/// TODO: We're ignoring the suffix
struct DecimalIntegerLiteral {
  spDecimalNumeral decNumeral;
  //IntegerTypeSuffix suffix;
  DecimalIntegerLiteral() {}
};

/// DecimalNumeral:
///   0 | NonZeroDigit [Digits] | [1-9] Underscores Digits
/// Where
///   NonZeroDigit: One of 1..9
///   Digits: Digit | Digit [DigitsAndUnderscores] Digit
///   Underscores: _ {_}
/// TODO: We're not dealing with underscores
struct DecimalNumeral {
  int pos, len;
  long decimal;
};

struct Error {
  int ini, end, type;
  Error(int ini, int end, int type) : ini(ini), end(end), type(type) {}
};

}
#endif
