#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_UNARYOPERATORINVERSIONCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_UNARYOPERATORINVERSIONCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Unary Operator Inversion for if statements
class UnaryOperatorInversionCheck : public ClangTidyCheck {
public:
  UnaryOperatorInversionCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_UNARYOPERATORINVERSIONCHECK_H
