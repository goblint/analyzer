#include "RemoveThreadWrapperCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void RemoveThreadWrapperCheck::registerMatchers(MatchFinder *Finder) {
  if (WrapFunctionName.empty()) {
    exit(-1);
  } else {
    Finder->addMatcher(functionDecl(hasName(WrapFunctionName), isDefinition()).bind("wrap_function"), this);
  }
}

void RemoveThreadWrapperCheck::check(const MatchFinder::MatchResult &Result) {
  auto *MatchedDecl = Result.Nodes.getNodeAs<FunctionDecl>("wrap_function");
    
    std::string Replacement = "\nint " + WrapFunctionName + "_wrap(void *arg) {\n"
                              + "\t/*[MUTATION][RTW]  Wrapped function for remove-thread */\n"
                              + "\t" + WrapFunctionName + "(arg);\n"
                              + "\treturn 0;\n}";

    // Get locations
    SourceLocation Start = MatchedDecl->getEndLoc().getLocWithOffset(1);
    SourceLocation End = MatchedDecl->getEndLoc().getLocWithOffset(1);
    auto Range = CharSourceRange::getCharRange(Start, End);
    diag(Start, "[MUTATION][RTW] Function %0 has been wrapped")
        << MatchedDecl
        << FixItHint::CreateReplacement(Range, Replacement);
}

void RemoveThreadWrapperCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "WrapFunctionName", WrapFunctionName);
}

} // namespace clang::tidy::readability
