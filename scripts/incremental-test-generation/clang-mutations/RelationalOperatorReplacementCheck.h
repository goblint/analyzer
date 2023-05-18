#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_RELATIONALOPERATORREPLACEMENTCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_RELATIONALOPERATORREPLACEMENTCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Relational Operator Replacement
class RelationalOperatorReplacementCheck : public ClangTidyCheck {
public:
  RelationalOperatorReplacementCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_RELATIONALOPERATORREPLACEMENTCHECK_H
