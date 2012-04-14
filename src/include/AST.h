//-*- C++ -*-
#ifndef __ANNOTATIONS_H__
#define __ANNOTATIONS_H__
#include <vector>
#include <boost/shared_ptr.hpp>

namespace djp {

typedef boost::shared_ptr<struct CompilationUnit> spCompilationUnit;
typedef boost::shared_ptr<struct PackageDeclaration> spPackageDeclaration;
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

/// CompilationUnit:
///   [PackageDeclaration]
///   {ImportDeclarations}
///   {TypeDeclarations}
struct CompilationUnit {
  spPackageDeclaration pkgDecl;
  // ImportDeclarations impDecls;
  // TypeDeclarations typeDecls;
};

/// PackageDeclaration: [ [Annotations]  package QualifiedIdentifier ; ]
struct PackageDeclaration {
  int pos, len;
  std::vector<spAnnotation> annotations;
  // package keyword
  // QualifiedIdentifier
};

/// Annotation: @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
struct Annotation {
  int pos, len, posTokAt;
  spQualifiedIdentifier qualifiedId;
  spAnnotationElement elem;
};

/// QualifiedIdentifier: Identifier { . Identifier }
struct QualifiedIdentifier {
  int pos;
  std::string value;
};

/// ElementValuePair: ElementValuePairs | ElementValue
/// TODO: they should be a union
struct AnnotationElement {
  std::vector<spElementValuePair> pairs;
  spElementValue value;
};

/// ElementValuePair: Identifier = ElementValue
struct ElementValuePair {
  spIdentifier id;
  spElementValue value;
};

/// TODO: probably string and position
struct Identifier {

};

/// TODO: Enable the commented structures as union
struct ElementValue {
  //Annotation annotation;
  spExpression1 expr1;
  //ElementValueArrayInitializer elemValArrayInit;
};

/// Expression1: Expression2 [Expression1Rest]
/// TODO: Expression1Rest
struct Expression1 {
  spExpression2 expr2;
  // [ Expression1Rest ]
};

/// Expression2: Expression3 [ Expression2Rest ]
/// TODO: Expression2Rest
struct Expression2 {
  spExpression3 expr3;
  // [ Expression2Rest ]
};

/// Expression3:
///   PrefixOp Expression3
///   ( Expression | Type ) Expression3
///   Primary { Selector } { PostfixOp }
/// TODO: deal with 1st and 3rd case using union
struct Expression3 {
  spPrimary primary; // TODO: { Selector } { PostfixOp }
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
  spIntegerLiteral intLit;
};

/// IntegerLiteral:
///   DecimalIntegerLiteral
///   HexIntegerLiteral
///   OctalIntegerLiteral
///   BinaryIntegerLiteral
/// TODO: We're only dealing with DecimalIntegerLiteral now
struct IntegerLiteral {
  spDecimalIntegerLiteral decIntLit;
};

/// DecimalIntegerLiteral: DecimalNumeral [IntegerTypeSuffix]
/// TODO: We're ignoring the suffix
struct DecimalIntegerLiteral {
  spDecimalNumeral decNumeral;
  //IntegerTypeSuffix suffix;
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

}
#endif
