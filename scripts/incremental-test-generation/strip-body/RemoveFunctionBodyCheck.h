#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVEFUNCTIONBODYCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVEFUNCTIONBODYCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Removes the body of a function and adds when needed generic return statements.
class RemoveFunctionBodyCheck : public ClangTidyCheck {
    const std::string RemoveOnlyFunctionName;

public:
  RemoveFunctionBodyCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context),
        RemoveOnlyFunctionName(Options.get("RemoveOnlyFunctionName", "")){}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVEFUNCTIONBODYCHECK_H
