import argparse
import os
import subprocess
import sys
from datetime import datetime
from pydriller import Repository
import yaml

sys.path.insert(0, "..")
from util.util import *

build_errors = 0
checkout_errors = 0
cil_errors = 0
ids_errors = []

def generate_git(goblint_path, temp_dir, meta_path, git_info_sh_path, start_commit, end_commit):
    goblint_path = os.path.expanduser(os.path.abspath(goblint_path))
    temp_dir = os.path.expanduser(os.path.abspath(temp_dir))
    temp_repo_dir = os.path.join(temp_dir, 'repo')
    meta_path = os.path.expanduser(os.path.abspath(meta_path))
    git_info_sh_path = os.path.expanduser(os.path.abspath(git_info_sh_path))

    print(SEPERATOR)
    print(f'[GIT] Cloning into {temp_repo_dir}')
    _clone_repo(git_info_sh_path, temp_repo_dir)
    build_path = _get_build_path(git_info_sh_path, temp_repo_dir)
    print(f'{COLOR_GREEN}[GIT] Cloning finished{COLOR_RESET}')

    if not os.path.exists(temp_repo_dir):
        os.mkdir(temp_repo_dir)

    def get_commit_traverser():
        all_commits = list(Repository(build_path).traverse_commits())
        if start_commit == None:
            start_index = 0
        else:
            start_index = next((i for i, commit in enumerate(all_commits) if commit.hash == start_commit), None)
        if end_commit == None:
            end_index = len(all_commits) - 1
        else:
            end_index = next((i for i, commit in enumerate(all_commits) if commit.hash == end_commit), None)

        if start_index is None or end_index is None:
            raise ValueError("One or both commit hashes not found in the repository")

        return all_commits[start_index:end_index + 1]


    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        index: int = yaml_data[META_N]

    num_of_commits = sum(1 for _ in get_commit_traverser())
    print(SEPERATOR)
    print(f'[GIT] Start traversing {num_of_commits} commits. Including checkout, build and cil generation. This may take a while...')
    if num_of_commits <= 2:
        print(f'{COLOR_RED}You must traverse at least two commits to generate a test!')
        sys.exit(-1)
    t = 0
    #last_failed = False
    for commit in get_commit_traverser():
        t += 1
        if commit.merge:
            print(f"[GIT][{t}/{num_of_commits}] {COLOR_YELLOW}Skipping merge commit {commit.hash}{COLOR_RESET}, continuing with the next commit...")
            last_failed = True
            continue
        try:
            _checkout(build_path, meta_path, commit.hash)
            _build_repo(git_info_sh_path, temp_repo_dir, meta_path, commit.hash)
            new_path = os.path.join(temp_dir, f"p_{index}.c")
            _create_cil_file(goblint_path, build_path, new_path, meta_path, commit.hash)
            
            #if t % 10 == 0 or last_failed:
                #last_failed = False
                #print(f"{COLOR_GREEN}[GIT][{t}/{num_of_commits}] Written cil file with index {index}{COLOR_RESET}, continuing with the next commits...")
            print(f"{COLOR_GREEN}[{t}/{num_of_commits}] Written cil file with index [{index}] for commit {commit.hash}{COLOR_RESET}, continuing with the next commits...")
            _write_meta_data(meta_path, commit.hash, index)
            index += 1
        except Exception as e:
            #last_failed = True
            print(f"{COLOR_RED}[{t}/{num_of_commits}][FAIL] Generating cil for commit {commit.hash} failed ({e}){COLOR_RESET}, continuing with the next commit...")
    print(f"{COLOR_GREEN}[FINISHED] Finished creating cil files for the commits.{COLOR_RESET}")
    if build_errors > 0 or checkout_errors > 0 or cil_errors > 0:
        print(f"{COLOR_RED}There were the following errors: {build_errors} build errors, {checkout_errors} checkout errors and {cil_errors} cil errors.{COLOR_RESET}")
        print(f"{COLOR_RED}You can read the error messages in the {meta_path} file")
        print(f"{COLOR_RED}The following commit ids resulted in errors:{COLOR_RESET} {', '.join(ids_errors)}")
    return index - 1
        

def _write_meta_data(meta_path, commit_hash, index):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    yaml_data[META_N] = index
    yaml_data[f"p_{index}"] = {
        META_TYPE: Generate_Type.GIT.value,
        META_SUB_TYPE: commit_hash
    }
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)

def _write_meta_data_failure(meta_path, commit_hash, stdout_msg, stderr_msg):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    yaml_data.setdefault(META_FAILURES, {})[commit_hash] = {
        META_FAILURES_STD_OUT: stdout_msg,
        META_FAILURES_STD_ERR: stderr_msg
    }
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)

def _clone_repo(git_info_sh_path, temp_repo_path):
    command = ["generators/generate_git_build.sh", git_info_sh_path, temp_repo_path, "--clone"]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode != 0:
         print(result.stdout)
         print(result.stderr)
         print(f"{COLOR_RED}Could not clone!{COLOR_RESET}")
         sys.exit(-1)

def _get_build_path(git_info_sh_path, temp_repo_path):
    command = ["generators/generate_git_build.sh", git_info_sh_path, temp_repo_path, "--path"]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode != 0:
        print(result.stdout)
        print(result.stderr)
        print(f"{COLOR_RED}Could not get build path!{COLOR_RESET}")
        sys.exit(-1)
    build_path = os.path.normpath(result.stdout.strip())
    return build_path

def _build_repo(git_info_sh_path, temp_repo_path, meta_path, commit_hash):
    global build_errors
    command = ["generators/generate_git_build.sh", git_info_sh_path, temp_repo_path, "--build"]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode != 0:
        build_errors += 1
        ids_errors.append(commit_hash)
        _write_meta_data_failure(meta_path, commit_hash, result.stdout, result.stderr)
        raise Exception("Could not build repo!")

def _checkout(build_path, meta_path, commit_hash):
    global checkout_errors
    # Clean untracked files
    clean_command = ['git', '-C', build_path, 'clean', '-f']
    clean_result = subprocess.run(clean_command, text=True, capture_output=True)
    if clean_result.returncode != 0:
        checkout_errors += 1
        ids_errors.append(commit_hash)
        _write_meta_data_failure(meta_path, commit_hash, clean_result.stdout, clean_result.stderr)
        raise Exception("Could not clean untracked files!")

    # Stash any uncommitted changes
    stash_command = ['git', '-C', build_path, 'stash']
    stash_result = subprocess.run(stash_command, text=True, capture_output=True)
    if stash_result.returncode != 0:
        checkout_errors += 1
        ids_errors.append(commit_hash)
        _write_meta_data_failure(meta_path, commit_hash, stash_result.stdout, stash_result.stderr)
        raise Exception("Could not stash changes!")

    # Checkout commit
    command = ['git', '-C', build_path, 'checkout', commit_hash]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode != 0:
        checkout_errors += 1
        ids_errors.append(commit_hash)
        _write_meta_data_failure(meta_path, commit_hash, result.stdout, result.stderr)
        raise Exception("Could not checkout repo!")

def _create_cil_file(goblint_path, build_path, output_path, meta_path, commit_hash):
    global cil_errors
    result = subprocess.run([goblint_path, '--set', 'justcil', 'true', '--set', 'cil.merge.inlines', 'false', build_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        cil_errors += 1
        ids_errors.append(commit_hash)
        _write_meta_data_failure(meta_path, commit_hash, result.stdout, result.stderr)
        raise Exception("Error creating cil!")
    with open(output_path, 'w') as f:
        f.write(result.stdout.decode())

if __name__ == "__main__":
    parser = argparse.ArgumentParser("Script for generating cil program files from commits")
    parser.add_argument("goblint_path", help="Path to Goblint directory")
    parser.add_argument("temp_dir", help="Path to the temporary directory")
    parser.add_argument("meta_path", help="Path to the meta directory")
    parser.add_argument("git_info_sh_path", help="Path to the Git information shell script")
    parser.add_argument("--start_commit", help="Hash id of the first commit to consider", default=None)
    parser.add_argument("--end_commit", help="Hash id of the last commit to consider", default=None)

    args = parser.parse_args()

    generate_git(args.goblint_path, args.temp_dir, args.meta_path, args.git_info_sh_path, args.start_commit, args.end_commit)
