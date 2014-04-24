//-*- C++ -*-
#ifndef __EMACS_OUTPUT_H__
#define __EMACS_OUTPUT_H__
#include <iostream>
#include <map>
#include <sstream>
#include "c4/Diagnosis.h"
#include "Indentation.h"
#include "AST.h"
#include "Parser.h"
#include "SymbolTable.h"
#include "Token.h"

namespace c4j {

/**
 * Emacs output for c4-mode.
 * The first position in the buffer is 1.
 */
class EmacsOutput {

  // Lexer and Parser products:
  spCompilationUnit compilationUnit;
  std::vector<spComment> comments;
  ST st;
  c4::spDiagnosis diag;
  LineIndentationMap &indentMap;

  TokenUtil tokenUtil;
  c4::ErrorUtil errUtil;

  // Symbol table types translation to elisp
  typedef std::map<int, std::string> STTypes;
  STTypes stTypes;

  // We distinguish references identifiers from other identifiers like variable
  // or method names for syntax highlighting.
  enum IdentifierOpt {
    OPT_UNDEFINED,
    OPT_IDENTIFIER_REFERENCE_TYPE,
  };

  void buildSyntaxHighlighting();
  void buildErrors();
  void buildSymbolTable();
  void buildIndentationTable();

  const std::string getSymbolTableType(int type);
  void setAnnotationElement(const spAnnotationElement elem);
  void setAnnotation(const spAnnotation &annotation);
  void setAnnotations(const std::vector<spAnnotation> &annotations);
  void setAnnotationMethodOrConstantRest(
    const spAnnotationMethodOrConstantRest &methodOrConstRest);
  void setAnnotationMethodRest(const spAnnotationMethodRest &methRest);
  void setAnnotationTypeBody(const spAnnotationTypeBody &annTypeBody);
  void setAnnotationTypeDeclaration(
    const spAnnotationTypeDeclaration &annotationDecl);
  void setAnnotationTypeElementDeclaration(
    const spAnnotationTypeElementDeclaration &elemDecl);
  void setAnnotationTypeElementDeclarations(
    const spAnnotationTypeElementDeclarations &elemDecls);
  void setAnnotationTypeElementRest(
    const spAnnotationTypeElementRest &elemRest);
  void setArguments(const spArguments &args);
  void setArrayCreatorRest(const spArrayCreatorRest &arrayCreatorRest);
  void setArrayDepth(ArrayDepth &arrayDepth);
  void setArrayInitializer(const spArrayInitializer arrayInit);
  void setBasicType(const spBasicType &basicType);
  void setBlock(const spBlock &block);
  void setBlockStatement(const spBlockStatement &blockStmt);
  void setBound(const spBound &bound);
  void setCatches(const spCatches &catches);
  void setCatchClause(const spCatchClause &catchClause);
  void setCatchType(const spCatchType &catchType);
  void setCharacterLiteral(const spCharacterLiteral &charLiteral);
  void setClassBody(const spClassBody &classBody);
  void setClassBodyDeclaration(const spClassBodyDeclaration &decl);
  void setClassCreatorRest(const spClassCreatorRest &classCreatorRest);
  void setClassDeclaration(const spClassDeclaration &classDecl);
  void setClassOrInterfaceDeclaration(
    const spClassOrInterfaceDeclaration &decl);
  void setComments();
  void setConstantDeclaratorRest(
    const spConstantDeclaratorRest &constDeclRest);
  void setConstantDeclaratorsRest(
    const spConstantDeclaratorsRest &constDeclsRest);
  void setConstructorDeclaratorRest(
    const spConstructorDeclaratorRest &constDeclRest);
  void setCreatedName(const spCreatedName &createdName);
  void setCreator(const spCreator &creator);
  void setElementValue(const spElementValue &value);
  void setElementValues(const spElementValues &values);
  void setElementValueArrayInitializer(
    const spElementValueArrayInitializer &elemValArrayInit);
  void setElementValuePair(const spElementValuePair &pair);
  void setEnumBody(const spEnumBody &enumBody);
  void setEnumBodyDeclarations(const spEnumBodyDeclarations &bodyDecls);
  void setEnumConstant(const spEnumConstant &enumConst);
  void setEnumConstants(const spEnumConstants &enumConsts);
  void setEnumDeclaration(spEnumDeclaration &enumDecl);
  void setErrors(const std::vector<c4::spError> &errors);
  void setExplicitGenericInvocation(
    const spExplicitGenericInvocation &explGenInvocation);
  void setExplicitGenericInvocationSuffix(
    const spExplicitGenericInvocationSuffix &explGen);
  void setExpression(const spExpression &expr);
  void setExpression1(const spExpression1 &expr1);
  void setExpression1Rest(const spExpression1Rest &expr1Rest);
  void setExpression2(const spExpression2 &expr2);
  void setExpression2Rest(const spExpression2Rest &expr2Rest);
  void setExpression3(const spExpression3 &expr3);
  void setFieldDeclsRest(const spFieldDeclaratorsRest &fieldDeclsRest);
  void setFinally(const spFinally &finally);
  void setFloatingPointLiteral(const spFloatingPointLiteral &fpLiteral);
  void setForControl(const spForControl &forCtrl);
  void setForInit(const spForInit &forInit);
  void setForUpdate(const spForUpdate &forUpdate);
  void setForVarControl(const spForVarControl &varCtrl);
  void setForVarControlRest(const spForVarControlRest &forVarCtrlRest);
  void setForVariableDeclaratorsRest(
    const spForVariableDeclaratorsRest &forVarDeclsRest);
  void setFormalParameters(const spFormalParameters &formParams);
  void setFormalParameterDecls(const spFormalParameterDecls &formParamDecls);
  void setFormalParameterDeclsRest(
    const spFormalParameterDeclsRest &formParamDeclsRest);
  void setGenericMethodOrConstructorDecl(
    const spGenericMethodOrConstructorDecl &genMethodOrConstDecl);
  void setGenericMethodOrConstructorRest(
    const spGenericMethodOrConstructorRest &rest);
  void setIdentifier(const spIdentifier &identifier,
    IdentifierOpt opt = EmacsOutput::OPT_UNDEFINED);
  void setIdentifierSuffix(const spIdentifierSuffix &idSuffix);
  void setImportDeclaration(const spImportDeclaration &import);
  void setImportDeclarations(const spImportDeclarations &impDecls);
  void setInnerCreator(const spInnerCreator &innerCreator);
  void setIntegerLiteral(const spIntegerLiteral &intLiteral);
  void setInterfaceBody(const spInterfaceBody &body);
  void setInterfaceBodyDeclaration(const spInterfaceBodyDeclaration &bodyDecl);
  void setInterfaceDeclaration(const spInterfaceDeclaration &interfaceDecl);
  void setInterfaceGenericMethodDecl(
    const spInterfaceGenericMethodDecl &genMethodDecl);
  void setInterfaceMemberDeclaration(const spInterfaceMemberDecl &memberDecl);
  void setInterfaceMethodDeclaratorRest(
    const spInterfaceMethodDeclaratorRest &methDeclRest);
  void setInterfaceMethodOrFieldDecl(
    const spInterfaceMethodOrFieldDecl &methodOrFieldDecl);
  void setInterfaceMethodOrFieldRest(const spInterfaceMethodOrFieldRest &rest);
  void setKeyword(int ini, int end);
  void setKeyword(const spTokenExp &token);
  void setLiteral(const spLiteral &literal);
  void setLocalVariableDeclarationStatement(
    const spLocalVariableDeclarationStatement &localVar);
  void setMemberDecl(const spMemberDecl &memberDecl);
  void setMethodDeclaratorRest(const spMethodDeclaratorRest &methodDeclRest);
  void setMethodOrFieldDecl(const spMethodOrFieldDecl &methodOrFieldDecl);
  void setMethodOrFieldRest(const spMethodOrFieldRest &methodOrFieldRest);
  void setModifier(const spModifier &modifier);
  void setNonWildcardTypeArguments(
    const spNonWildcardTypeArguments &nonWildcardTypeArguments);
  void setNonWildcardTypeArgumentsOrDiamond(
    const spNonWildcardTypeArgumentsOrDiamond &nonWildcardOrDiam);
  void setNormalClassDeclaration(const spNormalClassDeclaration &nClassDecl);
  void setNormalInterfaceDeclaration(
    const spNormalInterfaceDeclaration &normalDecl);
  void setOp(unsigned int ini, int len = 1);
  void setPackageDeclaration(const spPackageDeclaration &pkgDecl);
  void setParExpression(const spParExpression &parExpr);
  void setPrimary(const spPrimary &primary);
  void setPrimaryIdentifier(const spPrimaryIdentifier &primaryId);
  void setQualifiedId(const spQualifiedIdentifier &qualifiedId);
  void setQualifiedIdentifierList(
    const spQualifiedIdentifierList &qualifiedIdList);
  void setReferenceType(const spReferenceType &refType);
  void setResource(const spResource &res);
  void setResources(const spResources &resources);
  void setResourceSpecification(const spResourceSpecification &resSpec);
  void setSelector(const spSelector &selector);
  void setStatement(const spStatement &stmt);
  void setStatementExpression(const spStatementExpression &stmtExpr);
  void setStringLiteral(const spStringLiteral &strLiteral);
  void setSuperSuffix(const spSuperSuffix &superSuffix);
  void setSwitchBlockStatementGroup(const spSwitchBlockStatementGroup &group);
  void setSwitchLabel(const spSwitchLabel &label);
  void setSwitchLabels(const spSwitchLabels &labels);
  void setType(const spType &type);
  void setTypeArgument(const spTypeArgument &typeArg);
  void setTypeArguments(const spTypeArguments &typeArgs);
  void setTypeArgumentsOrDiamond(const spTypeArgumentsOrDiamond &typeArgsOrDiam);
  void setTypeDeclarations(const std::vector<spTypeDeclaration> &typeDecls);
  void setTypeList(const spTypeList &typeList);
  void setTypeList2(const spTypeList2 &typeList2);
  void setTypeParameter(const spTypeParameter &typeParam);
  void setTypeParameters(const spTypeParameters &typeParams);
  void setVariableDeclarator(const spVariableDeclarator &varDecl);
  void setVariableDeclaratorId(const spVariableDeclaratorId &varDeclId);
  void setVariableDeclaratorRest(const spVariableDeclaratorRest &varDeclRest);
  void setVariableDeclarators(const spVariableDeclarators &varDecls);
  void setVariableInitializer(const spVariableInitializer &varInit);
  void setVariableModifier(const spVariableModifier &varModifier);
  void setVoidInterfaceMethodDeclaratorRest(
    const spVoidInterfaceMethodDeclaratorRest &voidMethDeclRest);
  void setVoidMethodDeclaratorRest(
    const spVoidMethodDeclaratorRest &voidMethDeclRest);

public:
  // Output sent to emacs
  std::stringstream outSH; // syntax highlighting
  std::stringstream outErr;
  std::stringstream outST; // symbol table
  std::stringstream outIT; // indentation table

  EmacsOutput(Parser &parser)
    : compilationUnit(parser.compilationUnit), comments(parser.comments),
      st(parser.st), diag(parser.diag), indentMap(parser.indentMap)
  {
    // Symbol table types representation in elisp
    stTypes[ST_COMPILATION_UNIT] = "_comp_unit";
    stTypes[ST_PACKAGE] = "_package";
    stTypes[ST_CLASS_OR_INTERFACE] = "_class_or_interface";
    stTypes[ST_MEMBER_DECL] = "_member_decl";
    stTypes[ST_CLASS] = "_class";
    stTypes[ST_ENUM] = "_enum";
    stTypes[ST_INTERFACE] = "_interface";
    stTypes[ST_METHOD] = "_method";
    stTypes[ST_FIELD] = "_field";
    stTypes[ST_IDENTIFIER] = "id";
    stTypes[ST_TYPE] = "type";

    // TODO: we can infer the size of outIT from indentMap
  }

  void build();
};

}

#endif
