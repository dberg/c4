#include "Output.h"
#include "Token.h"

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

  output += ")";
}

void Output::setPackageDeclaration(spPackageDeclaration &pkgDecl) {
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

void Output::setImportDeclarations(spImportDeclarations &impDecls) {
  output += "(djp-import-declarations ";
  for (std::string::size_type i = 0; i < impDecls->imports.size(); i++) {
    setImportDeclaration(impDecls->imports[i]);
  }
  output += ")";
}

void Output::setImportDeclaration(spImportDeclaration &import) {
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

void Output::setTypeDeclarations(std::vector<spTypeDeclaration> &typeDecls) {
  for (std::size_t i = 0; i < typeDecls.size(); i++) {
    if (typeDecls[i]->decl) {
      setClassOrInterfaceDeclaration(typeDecls[i]->decl);
    }
  }
}

void Output::setClassOrInterfaceDeclaration(
  spClassOrInterfaceDeclaration &decl) {

  if (decl->modifier) {
    setModifier(decl->modifier);
  }
}

void Output::setModifier(spModifier &modifier) {
  if (modifier->annotations.size()) {
    setAnnotations(modifier->annotations);
  }

  for (std::size_t i = 0; i < modifier->tokens.size(); i++) {
    setKeyword(modifier->tokens[i]);
  }
}

void Output::setKeyword(spTokenExp &token) {
  setKeyword(token->pos + 1, token->pos + 1
    + tokenUtil.getTokenLength(token->type));
}

void Output::setKeyword(int ini, int end) {
  std::stringstream ini_s; ini_s << ini;
  std::stringstream end_s; end_s << end;
  output += "(djp-node-keyword " + ini_s.str() + " " + end_s.str() + ")";
}

void Output::setQualifiedId(int ini, int end) {
  std::stringstream ini_s; ini_s << ini;
  std::stringstream end_s; end_s << end;
  output += "(djp-node-qualified-id " + ini_s.str() + " " + end_s.str() + ")";
}

/// We return zero or more of the form:
/// (annotation INI ERR (qualified-id INI END))
/// where the annotation ini marks the position of the token '@' and we delimit
/// qualified-id as one element being the ini the first identifier and the end
/// the last identifier.
void Output::setAnnotations(std::vector<spAnnotation> &annotations) {
  for (unsigned int i = 0; i < annotations.size(); i++) {
    std::stringstream ann_ini; ann_ini << annotations[i]->posTokAt + 1;
    std::stringstream ann_err; ann_err << (int) annotations[i]->err;

    int ini = 0; int end = 0;
    if (annotations[i]->qualifiedId) {
      ini = annotations[i]->qualifiedId->ini + 1;
      end = annotations[i]->qualifiedId->end + 2;
    }

    std::stringstream qid_iniss; qid_iniss << ini;
    std::stringstream qid_endss; qid_endss << end;

    output += "(djp-node-annotation " + ann_ini.str() + " " + ann_err.str()
      + " (djp-node-qualified-id " + qid_iniss.str() + " " + qid_endss.str() + "))";
  }
}
} // namespace
