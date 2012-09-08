//-*- C++ -*-
#ifndef __PARSER_H__
#define __PARSER_H__
#include <string>
#include <sstream>
#include <vector>
#include "AST.h"
#include "Diagnosis.h"
#include "ErrorCodes.h"
#include "Lexer.h"
#include "SourceCodeStream.h"
#include "SymbolTable.h"
#include "Token.h"

namespace djp {
class Parser {
  const std::string filename;
  spSourceCodeStream src;
  spDiagnosis diag;
  spLexer lexer;
  std::vector<std::string> scopes;
  TokenUtil tokenUtil;

  // Parser
  spAnnotation parseAnnotation();
  void parseAnnotationElement(spAnnotationElement &elem);
  void parseAnnotations(std::vector<spAnnotation> &annotations);
  void parseArguments(spArguments &args);
  int parseArrayDepth();
  void parseBooleanLiteral(spBooleanLiteral &boolLit);
  void parseCharacterLiteral(spCharacterLiteral &charLit);
  void parseClassBody(spClassBody &classBody);
  void parseClassBodyDeclaration(spClassBodyDeclaration &decl);
  void parseClassCreatorRest(spClassCreatorRest &classCreatorRest);
  void parseClassDeclaration(spClassDeclaration &classDecl);
  void parseClassOrInterfaceDeclaration(spClassOrInterfaceDeclaration& decl);
  void parseCompilationUnit();
  void parseConstructorDeclaratorRest(
    spConstructorDeclaratorRest &constDeclRest);
  void parseCreatedName(spCreatedName &createdName);
  void parseCreatedNameHelper(spCreatedName &createdName);
  void parseCreator(spCreator &creator);
  void parseCreatorOpt1(spCreatorOpt1 &opt1);
  void parseCreatorOpt2(spCreatorOpt2 &opt2);
  void parseElementValue(spElementValue &value);
  void parseElementValuePairs(std::vector<spElementValuePair> &pairs);
  void parseExpression(spExpression &expr);
  void parseExpression1(spExpression1 &expr1);
  void parseExpression2(spExpression2 &expr2);
  void parseExpression3(spExpression3 &expr3);
  spImportDeclaration parseImportDeclaration();
  spImportDeclarations parseImportDeclarations();
  void parseFormalParameters(spFormalParameters &formParams);
  void parseFormalParameterDecls(spFormalParameterDecls &formParamDecls);
  void parseFormalParameterDeclsRest(
    spFormalParameterDeclsRest &formParamDeclsRest);
  void parseIntegerLiteral(spIntegerLiteral &intLiteral);
  void parseFloatingPointLiteral(spFloatingPointLiteral &fpLiteral);
  void parseLiteral(spLiteral &literal);
  void parseModifier(spModifier &modifier);
  void parseMemberDecl(spMemberDecl &memberDecl);
  void parseNonWildcardTypeArguments(
    spNonWildcardTypeArguments &nonWildcardTypeArguments);
  void parseNormalClassDeclaration(spNormalClassDeclaration &nClassDecl);
  void parseNullLiteral(spNullLiteral &nullLiteral);
  spPackageDeclaration parsePackageDeclaration(
    std::vector<spAnnotation> &annotations);
  void parsePairExpression(spPairExpression &pairExpr);
  void parsePrimary(spPrimary &primary);
  void parsePrimaryNewCreator(spPrimaryNewCreator &primaryNewCreator);
  void parsePrimarySuperSuperSuffix(
    spPrimarySuperSuperSuffix &primarySuperSuperSuffix);
  void parsePrimaryThisArguments(spPrimaryThisArguments &primaryThisArgs);
  spQualifiedIdentifier parseQualifiedIdentifier();
  void parseReferenceType(spReferenceType &refType);
  void parseStringLiteral(spStringLiteral &strLit);
  void parseSuperSuffix(spSuperSuffix &superSuffix);
  void parseType(spType &type);
  void parseTypeArgument(spTypeArgument &typeArg);
  void parseTypeArgumentOpt2(spTypeArgumentOpt2 &opt2);
  void parseTypeArguments(spTypeArguments &typeArgs);
  void parseTypeArgumentsOrDiamond(spTypeArgumentsOrDiamond &typeArgsOrDiam);
  std::vector<spTypeDeclaration> parseTypeDeclarations(
    std::vector<spAnnotation> &annotations);
  void parseTypeList(spTypeList &typeList);
  void parseVariableDeclaratorId(spVariableDeclaratorId &varDeclId);
  void parseVariableModifier(spVariableModifier &varModifier);

public:
  Parser(
    const std::string _filename, const std::string &_buffer, spDiagnosis &_diag)
    : filename(_filename),
      src(spSourceCodeStream(new SourceCodeStream(_buffer))),
      diag(_diag), lexer(spLexer(new Lexer(src, _diag))),
      compilationUnit(spCompilationUnit(new CompilationUnit())),
      error(0), error_msg("") {}

  spCompilationUnit compilationUnit;
  ST st;
  int error;
  std::string error_msg;

  void parse();
};
} // namespace

#endif
