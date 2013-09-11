#include "djp/ScalaSyntaxHighlighting.h"

namespace djp {

void ScalaSyntaxHighlighting::build() {
  // Compilation Unit
  sh << "[";
  // TODO: std::vector<{‘package’ QualId semi}>
  if (compUnit->topStatSeq) {
    setTopStatSeq(compUnit->topStatSeq);
  }

  sh << "]";
}

std::string ScalaSyntaxHighlighting::get() {
  return sh.str();
}

void ScalaSyntaxHighlighting::setTopStat(spTopStat &topStat) {
  if (topStat->opt == TopStat::Opt::TMPL_DEF) {
    // TODO:
    return;
  }

  if (topStat->opt == TopStat::Opt::IMPORT) {
    // TODO:
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGING) {
    // TODO:
    return;
  }

  if (topStat->opt == TopStat::Opt::PACKAGE_OBJECT) {
    // TODO:
    return;
  }
}

void ScalaSyntaxHighlighting::setTopStatSeq(spTopStatSeq &topStatSeq) {
  if (topStatSeq->topStat) {
    setTopStat(topStatSeq->topStat);
  }

  // TODO: std::vector<{semi TopStat}>
}

} // namespace