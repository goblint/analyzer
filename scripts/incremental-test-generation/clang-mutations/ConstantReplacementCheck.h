#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_CONSTANTREPLACEMENTCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_CONSTANTREPLACEMENTCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Constant Replacement
class ConstantReplacementCheck : public ClangTidyCheck {
public:
  ConstantReplacementCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_CONSTANTREPLACEMENTCHECK_H
