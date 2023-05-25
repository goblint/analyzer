#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADWRAPPERCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADWRAPPERCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Create wrapper for remove thread check
class RemoveThreadWrapperCheck : public ClangTidyCheck {
    const std::string WrapFunctionName;

public:
  RemoveThreadWrapperCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context),
      WrapFunctionName(Options.get("WrapFunctionName", "")) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADWRAPPERCHECK_H