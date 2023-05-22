#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADCHECK_H

#include "../ClangTidyCheck.h"

namespace clang::tidy::readability {

/// Remove Thread
class RemoveThreadCheck : public ClangTidyCheck {
public:
  RemoveThreadCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
private:
  std::string parseFunctionName(std::string FunctionName);
  std::string parseArgument(std::string Argument);
};

} // namespace clang::tidy::readability

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_READABILITY_REMOVETHREADCHECK_H
