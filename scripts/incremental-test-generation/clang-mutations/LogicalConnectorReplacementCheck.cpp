#include "LogicalConnectorReplacementCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void LogicalConnectorReplacementCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(stmt(expr(binaryOperator())).bind("logical-connector-replacement"), this);
}

// "||" <-> "&&"
void LogicalConnectorReplacementCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *MatchedDecl = Result.Nodes.getNodeAs<BinaryOperator>("logical-connector-replacement");
  
  std::string Replacement;

  if (MatchedDecl->getOpcode() == BO_LOr) {
    // || -> &&
    Replacement = "&&";
  } else if (MatchedDecl->getOpcode() == BO_LAnd) {
    // && -> ||
    Replacement = "||";
  } else {
    return;
  }
  Replacement += " /* [MUTATION][LCR] Replaced Logical Connector */";
  
  // Get locations
  SourceLocation Start = MatchedDecl->getOperatorLoc();
  SourceLocation End = MatchedDecl->getOperatorLoc().getLocWithOffset(2);
  auto Range = CharSourceRange::getCharRange(Start, End);

  diag(Start, "[MUTATION][LCR] Replaced Logical Connector %0")
      << MatchedDecl->getOpcodeStr()
      << FixItHint::CreateReplacement(Range, Replacement);
}

} // namespace clang::tidy::readability
