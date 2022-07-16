#!/usr/bin/python
import dataclasses
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
from os.path import isdir
from pathlib import Path
from pycparser import c_ast, c_parser, parse_file
from pycparser.c_ast import TypeDecl, ArrayDecl, PtrDecl, IdentifierType
from pycparser.c_generator import CGenerator

parser_errors = 0
struct_occurrences = 0
skips = 0
includes = 0
includes_only_assert = 0
invalid_solver = 0
introduced_changes = 0
renamed_a_function = 0

# to support library headers, first clone https://github.com/eliben/pycparser to the directory next of the analyzer folder.
# Then comment the lines out and in that are described that way.

def main():
    regression_folder = Path("./tests/regression")

    task = TaskRenameLocals(False)

    test = regression_folder / "25-vla/02-loop.c"
    execute_validation_test(test.parent, test, task)
    return

    excluded = [
        "44-trier_analyzer/33-recA.c",
        # Even though the same file is read in, the type of rec#i differes from int * to int?!
        "04-mutex/53-kernel-spinlock.c",  # Kernel is broken.
        "56-witness/01-base-lor-enums.c",  # 0evals?
        "56-witness/02-base-lor-addr.c",  # 0evals?
        "56-witness/03-int-log-short.c",  # 0evals?
        "56-witness/04-base-priv-sync-prune.c",  # 0evals?
        "44-trier_analyzer/09-G1.c",  # Also renamed glob var
        "44-trier_analyzer/21-Pproc.c"  # renamed function.
    ]

    # folder = regression_folder / "07-uninit"
    # for testFile in folder.iterdir():
    #     filename, extension = os.path.splitext(testFile.name)
    #     identifier = f"{folder.name}/{testFile.name}"
    #
    #     if extension == ".c" and not (identifier in excluded):
    #         execute_validation_test(folder, testFile)

    total_tests = 0
    executed_tests = 0

    for folder in regression_folder.iterdir():
        if isdir(folder):
            for testFile in folder.iterdir():
                filename, extension = os.path.splitext(testFile.name)
                if extension == ".c" and not (f"{folder.name}/{testFile.name}" in excluded):
                    total_tests += 1
                    if execute_validation_test(folder, testFile, task):
                        executed_tests += 1

    global introduced_changes
    global renamed_a_function

    print(f"Executed {executed_tests}/{total_tests}")
    if isinstance(task, TaskRenameLocals) and task.introduce_changes:
        print(f"Introduced changes in {introduced_changes}/{executed_tests}")

    if isinstance(task, TaskRenameFunction):
        print(f"Renamed a function in {renamed_a_function}/{executed_tests}")

    global parser_errors
    global struct_occurrences
    global skips
    global includes
    global invalid_solver
    global includes_only_assert

    print("Skipped due tue:")
    print("Parser errors: " + str(parser_errors))
    print("Struct occurrences: " + str(struct_occurrences))
    print("Skips (//Skip): " + str(skips))
    print(f"Includes: {includes}, of those only assert: {includes_only_assert}")
    print("Invalid solver: " + str(invalid_solver))


def execute_validation_test(folder: Path, test_file: Path, task):
    print(f"Executing test: {folder.name}/{test_file.name}")

    global parser_errors
    global struct_occurrences
    global skips
    global includes
    global invalid_solver
    global includes_only_assert
    global introduced_changes
    global renamed_a_function

    extra_params = ""

    with open(test_file, "r") as filehandle:
        lines = filehandle.readlines()
        if lines[0].startswith("// PARAM:"):
            extra_params = lines[0][len("// PARAM:"):-1]
        if lines[0].startswith("// SKIP"):
            print("Skipped test.")
            skips += 1
            return False
        # comment this if out if you want to support library headers
        if any(x.startswith("#include") for x in lines):
            print("Skipped test because of include")
            includes += 1

            include_lines = [x for x in lines if x.startswith("#include")]

            if all("assert.h" in x for x in include_lines):
                includes_only_assert += 1

            return False
        if any("struct" in x for x in lines):
            print("Skipped because struct")
            struct_occurrences += 1
            return False

    if "slr3" in extra_params or "slr4" in extra_params:
        print("Aborted test due to invalid solver.")
        invalid_solver += 1
        return False

    modified_file_result = create_modified_file(test_file, task)

    if modified_file_result is None:
        print("Aborted test due to parsing error.")
        parser_errors += 1
        return False

    base = "./"

    args = f"--enable dbg.debug --enable printstats -v {extra_params}"

    # uncomment to support library headers.
    # with tempfile.NamedTemporaryFile() as t:
    #     subprocess.run(f"cpp -E -I../pycparser/utils/fake_libc_include {test_file} > {t.name}", shell=True)
    #
    #
    #     x = subprocess.run(f"./goblint {args} --enable incremental.save {t.name}", shell=True, text=True, capture_output=True)
    #     if x.returncode != 0:
    #         includes += 1
    #         return False

    subprocess.run(f"./goblint {args} --enable incremental.save {test_file}", shell=True, capture_output=True)

    command = subprocess.run(
        f"./goblint {args} --enable incremental.load --set save_run {base}/{test_file}-incrementalrun {modified_file_result.tmp.name}",
        shell=True, text=True, capture_output=True)

    found_line = False

    for line in command.stdout.splitlines():
        if line.startswith("change_info = "):
            match = re.search("; changed = (\d+)", line)
            change_count = int(match.group(1))

            if modified_file_result.introduced_changes:
                invalid_change_count = change_count == 0
                expected = "> 0"
            else:
                invalid_change_count = change_count != 0
                expected = "= 0"

            if invalid_change_count != 0:
                print("-----------------------------------------------------------------")
                print(command.stdout)
                print("-----------------------------------------------------------------")
                print(f"Invalid change count={change_count}. Expected {expected}.")
                cleanup(folder, test_file, modified_file_result.tmp)
                sys.exit(-1)
            found_line = True
            break

    if not found_line:
        print("Could not find line with change count.")
        print(command.stdout)
        cleanup(folder, test_file, modified_file_result.tmp)
        sys.exit(-1)

    if modified_file_result.introduced_changes:
        introduced_changes += 1

    if modified_file_result.renamed_anything and isinstance(task, TaskRenameFunction):
        renamed_a_function += 1

    cleanup(folder, test_file, modified_file_result.tmp)

    return True


def cleanup(folder: Path, test: Path, updated_file):
    updated_file.close()
    shutil.rmtree(folder / f"{test.name}-incrementalrun")


def find_local_vars(node, on_node_found):
    if node.body.block_items is not None:
        for child in node.body.block_items:
            if isinstance(child, c_ast.Decl):
                if isinstance(child.type, c_ast.TypeDecl) or isinstance(child.type, c_ast.ArrayDecl):
                    on_node_found(child)


def rename_decl(node, new_name):
    if isinstance(node.type, TypeDecl) or isinstance(node.type, ArrayDecl) or isinstance(node.type, PtrDecl):
        node.name = new_name
        if isinstance(node.type, TypeDecl):
            node.type.declname = new_name
        if isinstance(node.type, ArrayDecl):
            node.type.type.declname = new_name
        if isinstance(node.type, PtrDecl):
            node.type.type.declname = new_name

def visit_rest_of_func_def(self, node):
    self.visit(node.decl)
    if node.param_decls is not None:
        self.visit(node.param_decls)

    self.visit(node.body)

class VarDeclVisitor(c_ast.NodeVisitor):

    def __init__(self):
        self.local_variables = {}
        self.function_params = {}

    def visit_FuncDef(self, node):
        lv = []
        fp = []

        find_local_vars(node, lambda f: lv.append(f.name))
        if isinstance(node.decl, c_ast.Decl) and isinstance(node.decl.type, c_ast.FuncDecl):
            func_decl = node.decl.type
            if isinstance(func_decl.args, c_ast.ParamList):
                for arg in func_decl.args.params:
                    if isinstance(arg, c_ast.Decl):
                        fp.append(arg.name)

        self.local_variables[node.decl.name] = lv
        self.function_params[node.decl.name] = fp


class RenameVariableVisitor(c_ast.NodeVisitor):

    def __init__(self, rename_mapping):
        self.map = rename_mapping

    def visit_ID(self, node):
        if node.name in self.map:
            node.name = self.map[node.name]

    def visit_Decl(self, node):
        if node.name in self.map:
            rename_decl(node, self.map[node.name])

        if node.init is not None:
            self.visit(node.init)

        self.visit(node.type)


class IntroduceSemanticChangeVisitor(c_ast.NodeVisitor):

    # legal_local_variables: Only these variables may be used to introduce a change
    def __init__(self, legal_local_variables):
        self.in_fun = False
        self.fun_name = None

        self.introduced_change = False
        self.found_vars = []
        self.introduced_changes = []
        self.legal_local_variables = legal_local_variables

    def visit_ID(self, node):
        if self.in_fun:
            if any(found_var for found_var in self.found_vars if found_var.name == node.name):
                known_var = [found_var for found_var in self.found_vars if found_var.name == node.name][0]

                # check if we can find another already declared var with the same type
                other_decls = [var for var in self.found_vars if
                               var.type == known_var.type and
                               var.name != known_var.name and
                               var.name in self.legal_local_variables[self.fun_name]
                               ]

                # only introduce change if not already done so for this variable
                if len(other_decls) > 0 and known_var.name not in self.introduced_changes:
                    node.name = other_decls[0].name
                    self.introduced_change = True
                    self.introduced_changes.append(known_var.name)
                else:
                    node.name = known_var.name


    def visit_FuncDef(self, node):
        self.in_fun = True
        self.fun_name = node.decl.name
        self.found_vars = []
        self.introduced_changes = []
        visit_rest_of_func_def(self, node)
        self.in_fun = False
        self.fun_name = None

    def visit_Decl(self, node):
        if self.in_fun and isinstance(node.type, c_ast.TypeDecl) or isinstance(node.type, c_ast.ArrayDecl):
            if isinstance(node.type, TypeDecl) and isinstance(node.type.type, IdentifierType):
                if len(node.type.type.names) == 1:
                    self.found_vars.append(LocalVar(node.name, node.type.type.names[0], node.name + "_updated"))
        if node.init is not None:
            self.visit(node.init)

        self.visit(node.type)


# find a single function to rename, but never main
class FindFunctionToRenameVisitor(c_ast.NodeVisitor):

    def __init__(self):
        self.fun_name = None
        self.updated_fun_name = None


    def visit_FuncDef(self, node):
        fun_name = node.decl.name
        if fun_name != "main" and self.fun_name is None:
            self.fun_name = fun_name
            self.updated_fun_name = fun_name + "_updated"


class RenameFunctionVisitor(c_ast.NodeVisitor):

    def __init__(self, function_to_rename_name, updated_name):
        self.function_to_rename_name = function_to_rename_name
        self.updated_name = updated_name

    def visit_FuncDef(self, node):
        fun_name = node.decl.name
        if fun_name == self.function_to_rename_name:
            node.decl.name = self.updated_name
            node.decl.type.type.declname = self.updated_name

        visit_rest_of_func_def(self, node)


    def visit_ID(self, node):
        if node.name == self.function_to_rename_name:
            node.name = self.updated_name


def create_modified_file(source_file: Path, task):
    try:
        # uncommet to support library headers.
        # gcc = subprocess.run(f"cpp -E -I../pycparser/utils/fake_libc_include {source_file}", shell=True, capture_output=True, text=True)

        # ast = c_parser.CParser().parse(gcc.stdout)
        ast = parse_file(source_file, use_cpp=True)

        introduced_change = False
        renamed_anything = False

        if isinstance(task, TaskRenameLocals):
            v = VarDeclVisitor()
            v.visit(ast)

            rename_mapping = {}
            local_vars = [x for xs in (list(v.local_variables.values()) + list(v.function_params.values())) for x in xs]
            for local_var in local_vars:
                rename_mapping[local_var] = local_var + "_updated"

            if task.introduce_changes:
                x = IntroduceSemanticChangeVisitor(v.local_variables)
                x.visit(ast)

                # print(CGenerator().visit(ast))
                # print("Introduced change:" + str(x.introduced_change))

                introduced_change = x.introduced_change
            else:
                introduced_change = False

            RenameVariableVisitor(rename_mapping).visit(ast)
            renamed_anything = len(local_vars) > 0

        if isinstance(task, TaskRenameFunction):
            v = FindFunctionToRenameVisitor()
            v.visit(ast)

            renamed_anything = v.fun_name is not None

            if v.fun_name is not None:
                v = RenameFunctionVisitor(v.fun_name, v.updated_fun_name)
                v.visit(ast)

            introduced_change = False

        # print(CGenerator().visit(ast))

        tmp = tempfile.NamedTemporaryFile()
        with open(tmp.name, "w") as f:
            f.write(CGenerator().visit(ast))

        return ModifiedFileResult(tmp, introduced_change, renamed_anything)
    except:
        return None


@dataclasses.dataclass
class ModifiedFileResult:
    tmp: tempfile.NamedTemporaryFile
    introduced_changes: bool
    renamed_anything: bool


@dataclasses.dataclass
class LocalVar:
    name: str
    type: str
    new_name: str


@dataclasses.dataclass
class TaskRenameLocals:
    introduce_changes: bool


@dataclasses.dataclass
class TaskRenameFunction:
    def __init__(self):
        self


if __name__ == '__main__':
    # result = create_modified_file(Path("scripts/test.c"), TaskRenameFunction())
    # print(result.introduced_changes)
    # result.tmp.close()
    main()
