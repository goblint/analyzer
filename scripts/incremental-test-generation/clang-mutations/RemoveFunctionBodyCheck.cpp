#include "RemoveFunctionBodyCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include <sstream>

using namespace clang::ast_matchers;

namespace clang::tidy::readability {

void RemoveFunctionBodyCheck::registerMatchers(MatchFinder *Finder) {
    if (RemoveOnlyFunctionName.empty()) {
        Finder->addMatcher(functionDecl(isDefinition()).bind("remove_function_body"), this);
    } else {
        std::vector<std::string> names;
        std::istringstream iss(RemoveOnlyFunctionName);
        for (std::string s; std::getline(iss, s, ','); ) {
            // trim leading and trailing whitespace from function name
            s.erase(0, s.find_first_not_of(" "));
            s.erase(s.find_last_not_of(" ") + 1);
            if (!s.empty()) {
                names.push_back(s);
            }
        }
        for (const auto& name : names) {
            Finder->addMatcher(functionDecl(hasName(name), isDefinition()).bind("remove_function_body"), this);
        }
    }
}

void RemoveFunctionBodyCheck::check(const MatchFinder::MatchResult &Result) {
    auto *MatchedDecl = Result.Nodes.getNodeAs<FunctionDecl>("remove_function_body");
    
    // Remove the function body
    std::string Replacement = " { ";
    const auto ReturnType = MatchedDecl->getReturnType();
    if (!ReturnType->isVoidType()) {
        Replacement += "return ";
        if (ReturnType->isPointerType() || ReturnType->isNullPtrType()) {
            Replacement += "0";
        } else if (ReturnType->isIntegralType(*Result.Context) || ReturnType->isCharType()) {
            Replacement += "0";
        } else if (ReturnType->isFloatingType()) {
            Replacement += "0.0";
        } else if (const RecordType *RT = ReturnType->getAsStructureType()) {
            Replacement += "(struct " + RT->getDecl()->getNameAsString() + "){}";
        } else if (const RecordType *RT = ReturnType->getAsUnionType()) {
            Replacement += "(union " + RT->getDecl()->getNameAsString() + "){}";
        } else {
            Replacement += "/* [TODO]: Add generic return value for " + ReturnType.getAsString() + " */";
        }
        Replacement += "; ";
    }
    Replacement += "/* [MUTATION][RFB] Stripped function of its body */ }";

    // Get locations
    SourceLocation Start = MatchedDecl->getTypeSpecEndLoc().getLocWithOffset(1);
    SourceLocation End = MatchedDecl->getBodyRBrace().getLocWithOffset(1);
    auto Range = CharSourceRange::getCharRange(Start, End);
    diag(Start, "[MUTATION][RFB] Function %0 has been stripped of its body")
        << MatchedDecl
        << FixItHint::CreateReplacement(Range, Replacement);
}

void RemoveFunctionBodyCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "RemoveOnlyFunctionName", RemoveOnlyFunctionName);
}

} // namespace clang::tidy::readability
