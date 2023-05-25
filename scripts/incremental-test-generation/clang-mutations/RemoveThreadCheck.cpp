#include "RemoveThreadCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include <sstream>

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void RemoveThreadCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(callExpr(callee(functionDecl(hasName("pthread_create")))).bind("remove-thread"), this);
}

void RemoveThreadCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("remove-thread");
  
  // Get locations
  SourceLocation Start = MatchedDecl->getBeginLoc();
  SourceLocation End = MatchedDecl->getEndLoc().getLocWithOffset(1);
  auto Range = CharSourceRange::getCharRange(Start, End);
  // Get the arguments
  const SourceManager *SM = Result.SourceManager;
  const char *StartPtr = SM->getCharacterData(Start);
  const char *EndPtr = SM->getCharacterData(End);
  size_t Length = EndPtr - StartPtr;
  std::string S(StartPtr, Length);
  std::vector<std::string> Arguments;
  std::istringstream iss(S);
  std::string argument;
  while (std::getline(iss, argument, ',')) {
      Arguments.push_back(argument);
  }
  // Get the function name
  std::string FunctionName = parseFunctionName(Arguments[2]);
  // Get the argument
  std::string Argument = parseArgument(Arguments[3]);
  // Create the replacement
  std::string Replacement = FunctionName + "_wrap(" + Argument + ") /* [MUTATION][RT][FUNCTION_NAME][" + FunctionName + "] Thread creation was substituted with function call */";
  diag(Start, "[MUTATION][RT][FUNCTION_NAME][%0] Thread creation was substituted with function call %0_wrapper")
      << FunctionName
      << FixItHint::CreateReplacement(Range, Replacement);
}

std::string RemoveThreadCheck::parseFunctionName(std::string FunctionName) {
  // Remove leading and trailing whitespaces
  FunctionName.erase(0, FunctionName.find_first_not_of(" \t"));
  FunctionName.erase(FunctionName.find_last_not_of(" \t") + 1);
  // Remove leading pointers
  FunctionName.erase(0, FunctionName.find_first_not_of("*"));
  // Remove leading dereference
  FunctionName.erase(0, FunctionName.find_first_not_of("&"));
  return FunctionName;
}

std::string RemoveThreadCheck::parseArgument(std::string Argument) {
  // Remove leading and trailing whitespaces
  Argument.erase(0, Argument.find_first_not_of(" \t"));
  Argument.erase(Argument.find_last_not_of(" \t") + 1);
  Argument.erase(Argument.find_last_of(')'), 1);
  return Argument;
}

} // namespace clang::tidy::readability
