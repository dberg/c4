#include "Output.h"

namespace djp {
void Output::build() {
  output = "(";
  if (compilationUnit->pkgDecl) {
    setPackageDeclaration(compilationUnit->pkgDecl);
  }

  if (compilationUnit->impDecls) {
    setImportDeclarations(compilationUnit->impDecls);
  }

  if (compilationUnit->typeDecls.size()) {
    setTypeDeclarations(compilationUnit->typeDecls);
  }

  if (compilationUnit->errors.size()) {
    setErrors(compilationUnit->errors);
  }

  output += ")";
}

/// We return zero or more of the form:
/// (annotation INI ERR (qualified-id INI END))
/// where the annotation ini marks the position of the token '@' and we delimit
/// qualified-id as one element being the ini the first identifier and the end
/// the last identifier.
void Output::setAnnotations(
  const std::vector<spAnnotation> &annotations) {

  for (std::size_t i = 0; i < annotations.size(); i++) {
    int ini = 0; int end = 0;
    if (annotations[i]->qualifiedId) {
      ini = annotations[i]->qualifiedId->ini + 1;
      end = annotations[i]->qualifiedId->end + 2;
    }

    output += "(djp-node-annotation "
      + itos(annotations[i]->posTokAt + 1) + " "
      + itos((int) annotations[i]->err)
      + " (djp-node-qualified-id "
      + itos(ini) + " "
      + itos(end) + "))";
  }
}

void Output::setBlock(const spBlock &block) {
  // TODO:
}

void Output::setClassBody(const spClassBody &classBody) {
  for (std::size_t i = 0; i < classBody->decls.size(); i++) {
    setClassBodyDeclaration(classBody->decls[i]);
  }
}

/// We have 2 options export:
///   (djp-member-decl-modifier-member-decl ...) -> {Modifier} MemberDecl
///   (djp-member-decl-modifier-static-block ...) -> [static] Block
void Output::setClassBodyDeclaration(const spClassBodyDeclaration &decl) {
  if (decl->opt == ClassBodyDeclaration::OPT_MODIFIER_MEMBER_DECL) {
    output += "(djp-member-decl-modifier-member-decl ";
    if (decl->modifier) {
      setModifier(decl->modifier);
    }

    if (decl->memberDecl) {
      setMemberDecl(decl->memberDecl);
    }

    output += ")";
    return;
  }

  // TODO:
  if (decl->opt == ClassBodyDeclaration::OPT_STATIC_BLOCK) {
    //output += "(djp-member-decl-modifier-static-block ";
    return;
  }
}

void Output::setClassOrInterfaceDeclaration(
  const spClassOrInterfaceDeclaration &decl) {

  output += "(djp-class-or-interface-declaration ";

  if (decl->modifier) {
    setModifier(decl->modifier);
  }

  if (decl->classDecl) {
    if (decl->classDecl->nClassDecl) {
      setNormalClassDeclaration(decl->classDecl->nClassDecl);
    } else if (decl->classDecl->enumDecl) {
      // TODO:
      //setEnumDeclaration(decl->classDecl->enumDecl);
    }
  }

  output += ")";
}

void Output::setConstructorDeclaratorRest(
  const spConstructorDeclaratorRest &constDeclRest) {
  output += "(djp-constructor-declarator-rest ";

  if (constDeclRest->formParams && constDeclRest->formParams->formParamDecls) {
    setFormalParameterDecls(constDeclRest->formParams->formParamDecls);
  }

  if (constDeclRest->block) {
    setBlock(constDeclRest->block);
  }

  output += ")";
}

void Output::setErrors(const std::vector<spError> &errors) {
  for (std::size_t i = 0; i < errors.size(); i++) {
    output += "(djp-error "
      + itos(errors[i]->ini + 1) + " "
      + itos(errors[i]->end + 1) + " \""
      + errUtil.getMessage(errors[i]->type) + "\")";

  }
}

void Output::setFormalParameterDecls(
  const spFormalParameterDecls &formParamDecls) {
  if (formParamDecls->varModifier) {
    setVariableModifier(formParamDecls->varModifier);
  }

  if (formParamDecls->type) {
    setType(formParamDecls->type);
  }

  if (formParamDecls->formParamDeclsRest) {
    setFormalParameterDeclsRest(formParamDecls->formParamDeclsRest);
  }
}

void Output::setFormalParameterDeclsRest(
  const spFormalParameterDeclsRest &formParamDeclsRest) {

  if (formParamDeclsRest->opt ==
    FormalParameterDeclsRest::OPT_VAR_DECL_ID) {
    setVariableDeclaratorId(formParamDeclsRest->varDeclId);
    if (formParamDeclsRest->formParamDecls) {
      setFormalParameterDecls(formParamDeclsRest->formParamDecls);
    }
    return;
  }

  if (formParamDeclsRest->opt ==
      FormalParameterDeclsRest::OPT_VAR_ARITY) {
    setVariableDeclaratorId(formParamDeclsRest->varDeclId);
  }
}

void Output::setIdentifier(const spIdentifier &identifier) {
  int ini = identifier->pos + 1;
  int end = ini + identifier->value.length();
  output += "(djp-node-identifier " + itos(ini) + " " + itos(end) + ")";
}

void Output::setImportDeclaration(const spImportDeclaration &import) {
  output += "(djp-import-declaration ";
  setKeyword(import->posTokImport + 1,
    import->posTokImport + tokenUtil.getTokenLength(TOK_KEY_IMPORT) + 1);

  if (import->posTokStatic > 0) {
    setKeyword(import->posTokStatic + 1,
      import->posTokStatic + tokenUtil.getTokenLength(TOK_KEY_STATIC) + 1);
    }

  if (import->qualifiedId) {
    int ini = import->qualifiedId->ini + 1;
    int end = import->qualifiedId->end + 2;
    if (import->iniOnDemand > 0) {
      end = import->endOnDemand + 2;
    }
    setQualifiedId(ini, end);
  }

  output += ")";
}

void Output::setImportDeclarations(const spImportDeclarations &impDecls) {
  output += "(djp-import-declarations ";
  for (std::string::size_type i = 0; i < impDecls->imports.size(); i++) {
    setImportDeclaration(impDecls->imports[i]);
  }
  output += ")";
}

void Output::setKeyword(const spTokenExp &token) {
  setKeyword(token->pos + 1, token->pos + 1
    + tokenUtil.getTokenLength(token->type));
}

void Output::setKeyword(int ini, int end) {
  output += "(djp-node-keyword " + itos(ini) + " " + itos(end) + ")";
}

void Output::setMemberDecl(const spMemberDecl &memberDecl) {
  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_METHOD_OR_FIELD_DECL) {
    return;
  }

  // TODO:
  if (memberDecl->opt ==
    MemberDecl::OPT_VOID_IDENTIFIER_VOID_METHOD_DECLARATOR_REST) {
    return;
  }

  if (memberDecl->opt ==
    MemberDecl::OPT_IDENTIFIER_CONSTRUCTOR_DECLARATOR_REST) {
    if (memberDecl->identifier) {
      setIdentifier(memberDecl->identifier);
    }

    if (memberDecl->constDeclRest) {
      setConstructorDeclaratorRest(memberDecl->constDeclRest);
    }
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_GENERIC_METHOD_OR_CONSTRUCTOR_DECL) {
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_CLASS_DECLARATION) {
    return;
  }

  // TODO:
  if (memberDecl->opt == MemberDecl::OPT_INTERFACE_DECLARATION) {
    return;
  }
}

void Output::setModifier(const spModifier &modifier) {
  if (modifier->annotations.size()) {
    setAnnotations(modifier->annotations);
  }

  for (std::size_t i = 0; i < modifier->tokens.size(); i++) {
    setKeyword(modifier->tokens[i]);
  }
}

void Output::setNormalClassDeclaration(
  const spNormalClassDeclaration &nClassDecl) {

  output += "(djp-normal-class-declaration ";

  if (nClassDecl->classTok) {
    setKeyword(nClassDecl->classTok);
  }

  if (nClassDecl->identifier) {
    setIdentifier(nClassDecl->identifier);
  }

  if (nClassDecl->classBody) {
    setClassBody(nClassDecl->classBody);
  }

  output += ")";
}

void Output::setPackageDeclaration(const spPackageDeclaration &pkgDecl) {
  output += "(djp-package-declaration ";
  setAnnotations(pkgDecl->annotations);

  // package keyword
  setKeyword(pkgDecl->pkgTokPos + 1,
    pkgDecl->pkgTokPos + tokenUtil.getTokenLength(TOK_KEY_PACKAGE) + 1);

  // package qualified identifier
  if (pkgDecl->qualifiedId) {
    setQualifiedId(pkgDecl->qualifiedId->ini + 1,
     pkgDecl->qualifiedId->end + 2);
  }

  output += ")";
}

void Output::setQualifiedId(int ini, int end) {
  output += "(djp-node-qualified-id "
    + itos(ini) + " " + itos(end) + ")";
}

void Output::setType(const spType &type) {
  if (type->opt == Type::OPT_BASIC_TYPE) {
    if (type->basicType->token) {
      setKeyword(type->basicType->token);
    }
    return;
  }

  if (type->opt == Type::OPT_REFERENCE_TYPE) {
    // TODO:
  }
}

void Output::setTypeDeclarations(
  const std::vector<spTypeDeclaration> &typeDecls) {

  for (std::size_t i = 0; i < typeDecls.size(); i++) {
    if (typeDecls[i]->decl) {
      setClassOrInterfaceDeclaration(typeDecls[i]->decl);
    }
  }
}

void Output::setVariableDeclaratorId(
  const spVariableDeclaratorId &varDeclId) {
  if (varDeclId->identifier) {
    setIdentifier(varDeclId->identifier);
  }
}

void Output::setVariableModifier(const spVariableModifier &varModifier) {
  if (varModifier->tokFinal) {
    setKeyword(varModifier->tokFinal);
  }

  setAnnotations(varModifier->annotations);
}

// Helper methods
const std::string Output::itos(int i) {
  std::stringstream s;
  s << i;
  return s.str();
}

} // namespace
