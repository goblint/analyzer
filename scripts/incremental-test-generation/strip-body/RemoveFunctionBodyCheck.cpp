/*

• sudo apt install ninja-build
• sudo apt install ccache
• sudo apt install lld
• "git clone https://github.com/llvm/llvm-project.git"
• Move to directory "llvm-project/clang-tools-extra"
• Run "clang-tidy/add_new_check.py readability remove-function-body"
• Replace "./clang-tidy/readability/RemoveFunctionBodyCheck.cpp" with the content of this file
• Update "./clang-tidy/readability/RemoveFunctionBodyCheck.h" with the the corresponding file in this directory
• "cd .." to llvm-project/
• "mkdir build"
• "cd build"
• "cmake -G "Ninja" -DLLVM_CCACHE_BUILD=ON -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=lld -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DLLVM_TARGETS_TO_BUILD="X86" -DLLVM_PARALLEL_COMPILE_JOBS=5 -DLLVM_PARALLEL_LINK_JOBS=5 ../llvm"
  Make sure to set the right build target (AMDGPU, ARM, AVR, BPF, Hexagon, Lanai, LoongArch, Mips, MSP430, NVPTX, PowerPC, RISCV, Sparc, SystemZ, VE, WebAssembly, X86, XCore)
  You can vary the linker and compiler jobs depending on your memory and CPU to improve the performance for "sudo ninja install"
  You can see the free memory with "free -h --giga"
• "sudo ninja install"
• Use "clang-tidy -checks=-*,readability-remove-function-body -fix --fix-errors -config="{CheckOptions: {readability-remove-function-body.RemoveOnlyFunctionName: ''}}" test.c --"
  For RemoveOnlyFunctionName you can use '' to strip all bodies. Alternativly use 'functionName' or 'functionName1, functionName2' to specify the target functions

*/

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
    std::string Replacement = " {\n\t// Stripped function of its body\n";
    const auto ReturnType = MatchedDecl->getReturnType();
    if (!ReturnType->isVoidType()) {
        Replacement += "\treturn ";
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
            Replacement += "/* TODO: Add generic return value for " + ReturnType.getAsString() + " */";
        }
        Replacement += ";\n";
    }
    SourceLocation Start = MatchedDecl->getTypeSpecEndLoc().getLocWithOffset(1);
    SourceLocation End = MatchedDecl->getBodyRBrace();
    auto Range = CharSourceRange::getCharRange(Start, End);
    diag(Start, "Function %0 has been stripped of its body")
        << MatchedDecl
        << FixItHint::CreateReplacement(Range, Replacement);
}

void RemoveFunctionBodyCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
    Options.store(Opts, "RemoveOnlyFunctionName", RemoveOnlyFunctionName);
}

} // namespace clang::tidy::readability
