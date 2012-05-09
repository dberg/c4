#include "Output.h"
#include "Token.h"

namespace djp {
void Output::print() {
  output = "(";
  if (compilationUnit->pkgDecl) {
    setPackageDeclaration(compilationUnit->pkgDecl);
  }
  output += ")";
  std::cout << output;
}

void Output::setPackageDeclaration(spPackageDeclaration &pkgDecl) {
  output += "(djp-package-declaration ";
  setAnnotations(pkgDecl->annotations);

  // package keyword
  std::stringstream ini; ini << pkgDecl->pkgTokPos + 1;
  std::stringstream end; end << pkgDecl->pkgTokPos + TOK_PACKAGE_LENGTH + 1;
  output += "(djp-node-keyword " + ini.str() + " " + end.str() + ")";

  // package qualified identifier
  if (pkgDecl->qualifiedId) {
    std::stringstream ini;
    ini << pkgDecl->qualifiedId->ini + 1;
    std::stringstream end;
    end << pkgDecl->qualifiedId->end + 2;
    output += "(djp-node-qualified-id " + ini.str() + " " + end.str() + ")";
  }

  output += ")";
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
