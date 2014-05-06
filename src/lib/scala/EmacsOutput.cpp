#include "c4/scala/EmacsOutput.h"

namespace c4s {

/**
 * Syntax highlighting, Errors and Indentation.
 */
void EmacsOutput::build() {
  auto shOutput = std::make_shared<SyntaxHighlighting>(compUnit, comments);
  shOutput->build();
  sh = shOutput->get();

  setErrors(diag->errors);
  setIndentation();
}

const std::string EmacsOutput::body() {
  std::stringstream body;
  body
    << sh << std::endl
    << errors << std::endl
    << indentation;

  return body.str();
}

void EmacsOutput::setErrors(const std::vector<c4::spError> &diagErrors) {
  std::stringstream ss;

  ss << "[";
  for (std::size_t i = 0; i < diagErrors.size(); i++) {
    ss << "("
      << (diagErrors[i]->ini + 1) << " "
      << (diagErrors[i]->end + 1) << " \""
      << errUtil.getMessage(diagErrors[i]->type) << "\")";
  }
  ss << "]";

  errors = ss.str();
}

void EmacsOutput::setIndentation() {
  std::stringstream ss;

  // Identation table
  // #s(hash-table size N data
  //   (LineNumber_N0 (IndentLevel_N0 LineWrap_N0 Offset_N0) ... N)
  ss << "#s(hash-table size " << indentMap.size() << " data (";

  LineIndentationMap::iterator it;
  for (it = indentMap.begin(); it != indentMap.end(); ++it) {
    ss << it->first << " (" << it->second->level << " "
      << ((it->second->lineWrap) ? "1" : "0") << " "
      << it->second->offset << ") ";
  }

  ss << "))";
  indentation = ss.str();
}

} // namespace
