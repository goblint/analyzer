#include "UnaryOperatorInversionCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void UnaryOperatorInversionCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(stmt(ifStmt()).bind("unary-operator-inversion"), this);
}

// Replace if(e) with if(!(e))
void UnaryOperatorInversionCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *MatchedDecl = Result.Nodes.getNodeAs<IfStmt>("unary-operator-inversion");
  // Get locations
  SourceLocation Start = MatchedDecl->getLParenLoc().getLocWithOffset(1);
  SourceLocation End = MatchedDecl->getRParenLoc();
  auto Range = CharSourceRange::getCharRange(Start, End);
  // Get the current expression in the if statement
  const SourceManager *SM = Result.SourceManager;
  const char *StartPtr = SM->getCharacterData(Start);
  const char *EndPtr = SM->getCharacterData(End);
  size_t Length = EndPtr - StartPtr;
  std::string Expression(StartPtr, Length);
  // Invert the expression with the unary operator
  std::string Replacement = "!(" + Expression + ")" + " /* [MUTATION][UOI] Inverted if statement */";
  diag(Start, "[MUTATION][UOI] If Statement %0 was inverted")
      << Expression
      << FixItHint::CreateReplacement(Range, Replacement);
}

} // namespace clang::tidy::readability
