import argparse
import copy
import os
import subprocess
import sys
from git import Repo
from datetime import datetime
import time
from pydriller import Repository
import yaml

sys.path.insert(0, "..")
from util.util import *

build_errors = 0
checkout_errors = 0
cil_errors = 0
ids_errors = []

def generate_git(goblint_path, temp_dir, meta_path, git_info_sh_path, start_window_ms, end_window_ms):
    goblint_path = os.path.expanduser(os.path.abspath(goblint_path))
    temp_dir = os.path.expanduser(os.path.abspath(temp_dir))
    temp_repo_dir = os.path.join(temp_dir, 'repo')
    meta_path = os.path.expanduser(os.path.abspath(meta_path))
    git_info_sh_path = os.path.expanduser(os.path.abspath(git_info_sh_path))

    print(SEPERATOR)
    print('[GIT] Cloning into temp/repo')
    _clone_repo(args.git_info_sh_path, temp_repo_dir)
    build_path = _get_build_path(args.git_info_sh_path, temp_repo_dir)

    repo = Repository(build_path)
    all_commits = list(repo.traverse_commits())
    if not all_commits:
        print(f"{COLOR_RED}No commits found in the repository{COLOR_RESET}")
        sys.exit(-1)
    earliest_commit = all_commits[0]
    latest_commit = all_commits[-1]
    if start_window_ms <= 0:
        start = datetime.fromtimestamp(earliest_commit.committer_date.timestamp())
    else:
        start = datetime.fromtimestamp(start_window_ms / 1000.0)
    if end_window_ms <= 0:
        end = datetime.fromtimestamp(latest_commit.committer_date.timestamp())
    else:
        end = datetime.fromtimestamp(end_window_ms / 1000.0)
    if not os.path.exists(temp_repo_dir):
        os.mkdir(temp_repo_dir)

    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        index: int = yaml_data[META_N]

    def get_commit_traverser():
        return Repository(build_path, since=start, to=end).traverse_commits()

    num_of_commits = sum(1 for _ in get_commit_traverser())
    print(f'[GIT] Start traversing {num_of_commits} commits. Including checkout, build and cil generation. This may take a while...')
    print(SEPERATOR)
    t = 0
    for commit in get_commit_traverser():
        t += 1
        if commit.merge:
            print(f"[GIT][{t}/{num_of_commits}] {COLOR_YELLOW}Skipping merge commit {commit.hash}{COLOR_RESET}, continuing with the next commit...")
            continue
        #TODO Skip small commits and summarize them in one big commit
        try:
            _checkout(build_path, meta_path, commit.hash)
            _build_repo(args.git_info_sh_path, temp_repo_dir, meta_path, commit.hash)
            new_path = os.path.join(temp_dir, f"p_{index}.c")
            _create_cil_file(goblint_path, build_path, new_path, meta_path, commit.hash)
            
            if t % 10 == 0:
                print(f"[GIT][{t}/{num_of_commits}] Written cil file with index {index}, continuing with the next commit...")
            _write_meta_data(meta_path, commit.hash, index)
            index += 1
        except Exception as e:
            print(f"{COLOR_RED}[GIT][{t}/{num_of_commits}][FAIL] Analysis for commit {commit.hash} failed ({e}){COLOR_RESET}, continuing with the next commit...")
    print(f"{COLOR_GREEN}[GIT][FINISHED] Finished creating cil files for the commits.{COLOR_RESET}")
    if build_errors > 0 or checkout_errors > 0 or cil_errors > 0:
        print(f"{COLOR_RED}There were the following errors: {build_errors} build errors, {checkout_errors} checkout errors and {cil_errors} cil errors.{COLOR_RESET}")
        print(f"{COLOR_RED}The following commit ids resulted in errors:{COLOR_RESET} {', '.join(ids_errors)}")
    return index
        

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
    command = ["./generate_git_build.sh", git_info_sh_path, temp_repo_path, "--clone"]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode != 0:
         print(result.stdout)
         print(result.stderr)
         print(f"{COLOR_RED}Could not clone!{COLOR_RESET}")
         sys.exit(-1)

def _get_build_path(git_info_sh_path, temp_repo_path):
    command = ["./generate_git_build.sh", git_info_sh_path, temp_repo_path, "--path"]
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
    command = ["./generate_git_build.sh", git_info_sh_path, temp_repo_path, "--build"]
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
    parser.add_argument("start_window_ms", help="Start of the time window in milliseconds", type=int)
    parser.add_argument("end_window_ms", help="End of the time window in milliseconds", type=int)

    args = parser.parse_args()

    generate_git(args.goblint_path, args.temp_dir, args.meta_path, args.git_info_sh_path, args.start_window_ms, args.end_window_ms)
