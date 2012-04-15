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
  CompilationUnit() : pkgDecl(spPackageDeclaration()) {}
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
  Annotation() : pos(-1), len(-1), posTokAt(-1),
    qualifiedId(spQualifiedIdentifier()),
    elem(spAnnotationElement()) {}
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
  AnnotationElement() : value(spElementValue()) {}
};

/// ElementValuePair: Identifier = ElementValue
struct ElementValuePair {
  spIdentifier id;
  spElementValue value;
  ElementValuePair(): id(spIdentifier()), value(spElementValue()) {}
};

/// TODO: probably string and position
struct Identifier {

};

/// TODO: Enable the commented structures as union
struct ElementValue {
  //Annotation annotation;
  spExpression1 expr1;
  //ElementValueArrayInitializer elemValArrayInit;
  ElementValue() : expr1(spExpression1()) {}
};

/// Expression1: Expression2 [Expression1Rest]
/// TODO: Expression1Rest
struct Expression1 {
  spExpression2 expr2;
  // [ Expression1Rest ]
  Expression1() : expr2(spExpression2()) {}
};

/// Expression2: Expression3 [ Expression2Rest ]
/// TODO: Expression2Rest
struct Expression2 {
  spExpression3 expr3;
  // [ Expression2Rest ]
  Expression2() : expr3(spExpression3()) {}
};

/// Expression3:
///   PrefixOp Expression3
///   ( Expression | Type ) Expression3
///   Primary { Selector } { PostfixOp }
/// TODO: deal with 1st and 3rd case using union
struct Expression3 {
  spPrimary primary; // TODO: { Selector } { PostfixOp }
  Expression3() : primary(spPrimary()) {}
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
  Primary() : literal(spLiteral()) {}
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
  Literal() : intLiteral(spIntegerLiteral()) {}
};

/// IntegerLiteral:
///   DecimalIntegerLiteral
///   HexIntegerLiteral
///   OctalIntegerLiteral
///   BinaryIntegerLiteral
/// TODO: We're only dealing with DecimalIntegerLiteral now
struct IntegerLiteral {
  spDecimalIntegerLiteral decIntLiteral;
  IntegerLiteral() : decIntLiteral(spDecimalIntegerLiteral()) {}
};

/// DecimalIntegerLiteral: DecimalNumeral [IntegerTypeSuffix]
/// TODO: We're ignoring the suffix
struct DecimalIntegerLiteral {
  spDecimalNumeral decNumeral;
  //IntegerTypeSuffix suffix;
  DecimalIntegerLiteral() : decNumeral(spDecimalNumeral()) {}
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
