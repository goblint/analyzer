#include "RelationalOperatorReplacementCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void RelationalOperatorReplacementCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(stmt(expr(binaryOperator())).bind("relational-operator-replacement"), this);
}

// ">=" <-> ">" and "=<" <-> "<"
void RelationalOperatorReplacementCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *MatchedDecl = Result.Nodes.getNodeAs<BinaryOperator>("relational-operator-replacement");
  
  int OperatorSize;
  std::string Replacement;

  if (MatchedDecl->getOpcode() == BO_GE) {
    // GE -> G
    OperatorSize = 2;
    Replacement = ">";
  } else if (MatchedDecl->getOpcode() == BO_GT) {
    // G -> GE
    OperatorSize = 1;
    Replacement = ">=";
  } else if (MatchedDecl->getOpcode() == BO_LE) {
    // LE -> L
    OperatorSize = 2;
     Replacement = "<";
  } else if (MatchedDecl->getOpcode() == BO_LT) {
    // LE -> L
    OperatorSize = 1;
     Replacement = "<=";
  } else {
    return;
  }
  Replacement += " /* [MUTATION][ROR] Replaced Relational Operator */";
  
  // Get locations
  SourceLocation Start = MatchedDecl->getOperatorLoc();
  SourceLocation End = MatchedDecl->getOperatorLoc().getLocWithOffset(OperatorSize);
  auto Range = CharSourceRange::getCharRange(Start, End);

  diag(Start, "[MUTATION][ROR] Replaced Relational Operator %0")
      << MatchedDecl->getOpcodeStr()
      << FixItHint::CreateReplacement(Range, Replacement);
}

} // namespace clang::tidy::readability
