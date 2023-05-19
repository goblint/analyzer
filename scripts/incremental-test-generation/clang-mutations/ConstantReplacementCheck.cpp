#include "ConstantReplacementCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void ConstantReplacementCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(integerLiteral(isExpansionInMainFile(), unless(anyOf(equals(0), equals(1)))).bind("constant-replacement"), this);
}

// Replaces all Integer Literals != 0 with 1
void ConstantReplacementCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *MatchedDecl = Result.Nodes.getNodeAs<IntegerLiteral>("constant-replacement");  

  // Get old int value
  llvm::APInt value = MatchedDecl->getValue();
  std::string valueAsString;
  llvm::SmallString<8> buffer;
  value.toString(buffer, 10, true);
  valueAsString = buffer.str();
  // Get locations
  SourceLocation Start = MatchedDecl->getBeginLoc();
  auto Range = MatchedDecl->getSourceRange();
  // Check if it is a Macro
  std::string MacroInfo = " ";
  if (MatchedDecl->getLocation().isMacroID()) {
    std::string MacroId = Lexer::getImmediateMacroName(MatchedDecl->getLocation(), *Result.SourceManager, Result.Context->getLangOpts()).str();
    MacroInfo = "[MACRO][" + MacroId + "] ";
  }
  // Replace with 1
  std::string Replacement = "1 /* [MUTATION][CR]" + MacroInfo + "Replaced Constant " + valueAsString + " */";
  diag(Start, "[MUTATION][CR]" + MacroInfo + "Replaced Constant %0 with 1")
    << valueAsString
    << FixItHint::CreateReplacement(Range, Replacement);
}

} // namespace clang::tidy::readability
