#include "Output.h"

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
  setAnnotations(pkgDecl->annotations);
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
      ini = annotations[i]->qualifiedId->pos + 1;
      end = ini + annotations[i]->qualifiedId->len;
    }

    std::stringstream qid_iniss; qid_iniss << ini;
    std::stringstream qid_endss; qid_endss << end;

    output += "(annotation " + ann_ini.str() + " " + ann_err.str()
      + " (qualified-id " + qid_iniss.str() + " " + qid_endss.str() + "))";
  }
}
} // namespace
