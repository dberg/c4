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
#include "ParserState.h"
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
  void buildParseTree();
  spAnnotation parseAnnotation();
  void parseAnnotationElement(spAnnotationElement &elem);
  void parseAnnotations(std::vector<spAnnotation> &annotations);
  void parseArguments(spArguments &args);
  void parseArrayCreatorRest(spArrayCreatorRest &arrayCreatorRest);
  void parseArrayCreatorRestOpt1(spArrayCreatorRestOpt1 &opt1);
  void parseArrayCreatorRestOpt2(spArrayCreatorRestOpt2 &opt2);
  void parseArrayDepth(ArrayDepth &arrayDepth);
  void parseArrayInitializer(spArrayInitializer &arrayInit);
  void parseBlock(spBlock &block);
  void parseBlockStatement(spBlockStatement &blockStmt);
  void parseBlockStatements(std::vector<spBlockStatement> &blockStmts);
  void parseBooleanLiteral(spBooleanLiteral &boolLit);
  void parseCatches(spCatches &catches);
  void parseCatchClause(spCatchClause &catchClause);
  void parseCatchType(spCatchType &catchType);
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
  void parseExplicitGenericInvocation(
    spExplicitGenericInvocation &explGenInvocation);
  void parseExplicitGenericInvocationSuffix(
    spExplicitGenericInvocationSuffix &explGen);
  void parseExpression(spExpression &expr);
  void parseExpression1(spExpression1 &expr1);
  void parseExpression2(spExpression2 &expr2);
  void parseExpression2Rest(spExpression2Rest &expr2Rest);
  void parseExpression3(spExpression3 &expr3);
  void parseFinally(spFinally &finally);
  void parseIdentifierSuffix(spIdentifierSuffix &idSuffix);
  void parseIdentifierSuffixOpt1Helper(spIdentifierSuffix &idSuffix);
  spImportDeclaration parseImportDeclaration();
  spImportDeclarations parseImportDeclarations();
  void parseInnerCreator(spInnerCreator &innerCreator);
  void parseFieldDeclaratorsRest(spFieldDeclaratorsRest &fieldDeclsRest);
  void parseFormalParameters(spFormalParameters &formParams);
  void parseFormalParameterDecls(spFormalParameterDecls &formParamDecls);
  void parseFormalParameterDeclsRest(
    spFormalParameterDeclsRest &formParamDeclsRest);
  void parseIntegerLiteral(spIntegerLiteral &intLiteral);
  void parseFloatingPointLiteral(spFloatingPointLiteral &fpLiteral);
  void parseLiteral(spLiteral &literal);
  void parseLocalVariableDeclarationStatement(
    spLocalVariableDeclarationStatement &localVar);
  void parseModifier(spModifier &modifier);
  void parseMemberDecl(spMemberDecl &memberDecl);
  void parseMethodDeclaratorRest(spMethodDeclaratorRest &methodDeclRest);
  void parseMethodOrFieldDecl(spMethodOrFieldDecl &methodOrFieldDecl);
  void parseMethodOrFieldRest(spMethodOrFieldRest &methodOrFieldRest);
  void parseNonWildcardTypeArguments(
    spNonWildcardTypeArguments &nonWildcardTypeArguments);
  void parseNonWildcardTypeArgumentsOrDiamond(
    spNonWildcardTypeArgumentsOrDiamond &nonWildcardOrDiam);
  void parseNormalClassDeclaration(spNormalClassDeclaration &nClassDecl);
  void parseNullLiteral(spNullLiteral &nullLiteral);
  spPackageDeclaration parsePackageDeclaration(
    std::vector<spAnnotation> &annotations);
  void parseParExpression(spParExpression &parExpr);
  void parsePostfixOp(spPostfixOp &postfixOp);
  void parsePrimary(spPrimary &primary);
  void parsePrimaryBasicType(spPrimaryBasicType &primaryBasicType);
  void parsePrimaryIdentifier(spPrimaryIdentifier &primaryId);
  void parsePrimaryNewCreator(spPrimaryNewCreator &primaryNewCreator);
  void parsePrimaryNonWildcardTypeArguments(
    spPrimaryNonWildcardTypeArguments &primaryNonWildcard);
  void parsePrimarySuperSuperSuffix(
    spPrimarySuperSuperSuffix &primarySuperSuperSuffix);
  void parsePrimaryThisArguments(spPrimaryThisArguments &primaryThisArgs);
  void parsePrimaryVoidClass(spPrimaryVoidClass &primaryVoidClass);
  spQualifiedIdentifier parseQualifiedIdentifier();
  void parseReferenceType(spReferenceType &refType);
  void parseSelector(spSelector &selector);
  void parseStatement(spStatement &stmt);
  void parseStatementExpression(spStatementExpression &stmtExpr);
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
  void parseVariableDeclaratorRest(spVariableDeclaratorRest &varDeclRest);
  void parseVariableDeclarator(spVariableDeclarator &varDecl);
  void parseVariableDeclarators(spVariableDeclarators &varDecls);
  void parseVariableInitializer(spVariableInitializer &varInit);
  void parseVariableModifier(spVariableModifier &varModifier);
  void parseVoidMethodDeclaratorRest(
    spVoidMethodDeclaratorRest &voidMethDeclRest);

  // Helper methods
  void saveState(State &state);
  void restoreState(State &state);

public:
  Parser(
    const std::string _filename, const std::string &_buffer, spDiagnosis &_diag)
    : filename(_filename),
      src(spSourceCodeStream(new SourceCodeStream(_buffer))),
      diag(_diag), lexer(spLexer(new Lexer(src, _diag))),
      compilationUnit(spCompilationUnit(new CompilationUnit())),
      error(0), error_msg("") {}

  spCompilationUnit compilationUnit;
  std::vector<spComment> comments;
  ST st;
  int error;
  std::string error_msg;

  void parse();
};
} // namespace

#endif
