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

// Helper functions
bool isAssignmentOperator(int token);
bool isBasicType(int token);
bool isDecimalIntegerLiteral(int token);
bool isHexIntegerLiteral(int token);
bool isOctalIntegerLiteral(int token);
bool isBinaryIntegerLiteral(int token);
bool isIntegerLiteral(int token);
bool isFloatingPointLiteral(int token);
bool isLiteral(int token);
bool isPrimary(int token);
bool isClassOrInterfaceDeclarationCandidate(int token);
bool isClassOrInterfaceModifier(int token);
bool isModifierToken(int token);
bool isInfixOp(int token);
bool isPrefixOp(int token);
bool isPostfixOp(int token);
bool isValidInitTokenOfClassBodyDeclaration(int token);
bool isValidInitTokenOfTypeDeclaration(int token);
bool isVariableModifier(int token);

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
  void parseBound(spBound &bound);
  void parseCatches(spCatches &catches);
  void parseCatchClause(spCatchClause &catchClause);
  void parseCatchType(spCatchType &catchType);
  void parseCharacterLiteral(spCharacterLiteral &charLit);
  void parseClassBody(spClassBody &classBody);
  void parseClassBodyDeclaration(spClassBodyDeclaration &decl);
  void parseClassBodyDeclarationsHelper(
    std::vector<spClassBodyDeclaration> &classBodyDecls);
  void parseClassCreatorRest(spClassCreatorRest &classCreatorRest);
  void parseClassDeclaration(spClassDeclaration &classDecl);
  void parseClassOrInterfaceDeclaration(spClassOrInterfaceDeclaration& decl);
  void parseCompilationUnit();
  void parseConstantDeclaratorsRest(spConstantDeclaratorsRest &constRest);
  void parseConstructorDeclaratorRest(
    spConstructorDeclaratorRest &constDeclRest);
  void parseCreatedName(spCreatedName &createdName);
  void parseCreatedNameHelper(spCreatedName &createdName);
  void parseCreator(spCreator &creator);
  void parseCreatorOpt1(spCreatorOpt1 &opt1);
  void parseCreatorOpt2(spCreatorOpt2 &opt2);
  void parseCreatorOpt3(spCreatorOpt3 &opt3);
  void parseElementValue(spElementValue &value);
  void parseElementValues(spElementValues &values);
  void parseElementValueArrayInitializer(
    spElementValueArrayInitializer &elemValArrayInit);
  void parseElementValuePairs(std::vector<spElementValuePair> &pairs);
  void parseEnumBody(spEnumBody &enumBody);
  void parseEnumBodyDeclarations(spEnumBodyDeclarations &bodyDecls);
  void parseEnumConstant(spEnumConstant &enumConst);
  void parseEnumConstants(spEnumConstants &enumConsts);
  void parseEnumDeclaration(spEnumDeclaration &enumDecl);
  void parseExplicitGenericInvocation(
    spExplicitGenericInvocation &explGenInvocation);
  void parseExplicitGenericInvocationSuffix(
    spExplicitGenericInvocationSuffix &explGen);
  void parseExpression(spExpression &expr);
  void parseExpression1(spExpression1 &expr1);
  void parseExpression1Rest(spExpression1Rest &expr1Rest);
  void parseExpression2(spExpression2 &expr2);
  void parseExpression2Rest(spExpression2Rest &expr2Rest);
  void parseExpression3(spExpression3 &expr3);
  void parseExpression3Opt2(spExpression3Opt2 &opt2);
  void parseExpression3Opt3(spExpression3Opt3 &opt3);
  void parseFieldDeclaratorsRest(spFieldDeclaratorsRest &fieldDeclsRest);
  void parseFinally(spFinally &finally);
  void parseFloatingPointLiteral(spFloatingPointLiteral &fpLiteral);
  void parseForControl(spForControl &forCtrl);
  void parseForInit(spForInit &forInit);
  void parseForUpdate(spForUpdate &forUpdate);
  void parseForVarControl(spForVarControl &forVarCtrl);
  void parseForVarControlRest(spForVarControlRest &forVarCtrlRest);
  void parseForVariableDeclaratorsRest(
    spForVariableDeclaratorsRest &forVarDeclsRest);
  void parseFormalParameters(spFormalParameters &formParams);
  void parseFormalParameterDecls(spFormalParameterDecls &formParamDecls);
  void parseFormalParameterDeclsRest(
    spFormalParameterDeclsRest &formParamDeclsRest);
  void parseGenericMethodOrConstructorDecl(
    spGenericMethodOrConstructorDecl &genMethodOrConstDecl);
  void parseGenericMethodOrConstructorRest(
    spGenericMethodOrConstructorRest &rest);
  void parseInterfaceBody(spInterfaceBody &body);
  void parseInterfaceBodyDeclaration(spInterfaceBodyDeclaration &bodyDecl);
  void parseInterfaceDeclaration(spInterfaceDeclaration &interfaceDecl);
  void parseInterfaceMemberDecl(spInterfaceMemberDecl &memberDecl);
  void parseInterfaceMethodDeclaratorRest(
    spInterfaceMethodDeclaratorRest &methDeclRest);
  void parseInterfaceMethodOrFieldDecl(
    spInterfaceMethodOrFieldDecl &methodOrFieldDecl);
  void parseInterfaceMethodOrFieldRest(spInterfaceMethodOrFieldRest &rest);
  void parseIdentifierSuffix(spIdentifierSuffix &idSuffix);
  spImportDeclaration parseImportDeclaration();
  spImportDeclarations parseImportDeclarations();
  void parseInnerCreator(spInnerCreator &innerCreator);
  void parseIntegerLiteral(spIntegerLiteral &intLiteral);
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
  void parseNormalInterfaceDeclaration(
    spNormalInterfaceDeclaration &normalDecl);
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
  void parseQualifiedIdentifier(spQualifiedIdentifier &qualifiedId);
  void parseQualifiedIdentifierList(spQualifiedIdentifierList &qualifiedIdList);
  void parseReferenceType(spReferenceType &refType);
  void parseSelector(spSelector &selector);
  void parseStatement(spStatement &stmt);
  void parseStatementExpression(spStatementExpression &stmtExpr);
  void parseStringLiteral(spStringLiteral &strLit);
  void parseSuperSuffix(spSuperSuffix &superSuffix);
  void parseSwitchBlockStatementGroup(spSwitchBlockStatementGroup &group);
  void parseSwitchBlockStatementGroups(
    spSwitchBlockStatementGroups &switchStmtGroups);
  void parseSwitchLabel(spSwitchLabel &label);
  void parseSwitchLabels(spSwitchLabels &labels);
  void parseType(spType &type);
  void parseTypeArgument(spTypeArgument &typeArg);
  void parseTypeArgumentOpt2(spTypeArgumentOpt2 &opt2);
  void parseTypeArguments(spTypeArguments &typeArgs);
  void parseTypeArgumentsOrDiamond(spTypeArgumentsOrDiamond &typeArgsOrDiam);
  std::vector<spTypeDeclaration> parseTypeDeclarations(
    std::vector<spAnnotation> &annotations);
  void parseTypeList(spTypeList &typeList);
  void parseTypeList2(spTypeList2 &typeList2);
  void parseTypeParameter(spTypeParameter &typeParam);
  void parseTypeParameters(spTypeParameters &typeParams);
  void parseVariableDeclaratorId(spVariableDeclaratorId &varDeclId);
  void parseVariableDeclaratorRest(spVariableDeclaratorRest &varDeclRest);
  void parseVariableDeclarator(spVariableDeclarator &varDecl);
  void parseVariableDeclarators(spVariableDeclarators &varDecls);
  void parseVariableInitializer(spVariableInitializer &varInit);
  void parseVariableModifier(spVariableModifier &varModifier);
  void parseVoidInterfaceMethodDeclaratorRest(
    spVoidInterfaceMethodDeclaratorRest &voidMethDeclRest);
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
      compilationUnit(spCompilationUnit(new CompilationUnit)),
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
