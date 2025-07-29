from argparse import ArgumentParser
from dataclasses import dataclass
import glob
from os import path
import shutil
from typing import List

try:
   from tqdm import tqdm
except:
    print("Install tdqm for nice progress bar")
    def tqdm(iterable, desc):
        print(desc)
        return iterable


@dataclass
class SetFile:
    path: str
    files: List[str]


def get_args():
    parser = ArgumentParser(
            prog="SVComp Sampler",
            description="Generate smaller subsets for quick tests")
    parser.add_argument("-s", "--svcomp-root", default="sv-benchmarks")
    parser.add_argument("-o", "--output", default="sampled-benchmarks")
    parser.add_argument("-f", "--factor", type=int, default=30)
    parser.add_argument("--skip-copy", action="store_true", help="Skip copying the svcomp repo and overwrite existing output")

    return parser.parse_args()


def find_set_files(svcomp_root):
    target_folder = path.join(svcomp_root, "c")
    return [path.join(target_folder, i) for i in glob.glob("*.set", root_dir=target_folder)]


def set_to_path_list(svcomp_root, setfile_path):
    c_root = path.join(svcomp_root, "c")
    with open(setfile_path, "r") as infile:
        lines = [i.strip() for i in infile.readlines()]
    lines = [path.join(c_root, i) for i in lines if not i.startswith("#") and i]
    expanded = [item for line in lines for item in glob.glob(line)]
    return expanded


def sample_setfile(setfile, factor):
    return SetFile(setfile.path,
                   setfile.files[0:len(setfile.files):factor])

def dump_output(svcomp_root, output, setfiles, skip_copy):
    if not skip_copy:
        print("Removing old output folder")
        shutil.rmtree(output, ignore_errors=True)
        print("Copying from svcomp")
        shutil.copytree(svcomp_root, output)
    for setfile in tqdm(setfiles, desc="Overwriting setfiles"):
        filename = path.basename(setfile.path)
        target_path = path.join(output, "c", filename)
        with open(target_path, "w") as ofile:
            ofile.writelines(setfile.files)


args = get_args()
set_file_paths = find_set_files(args.svcomp_root)
print(f"Found {len(set_file_paths)} setfiles")
setfiles = []
for setfile_path in tqdm(set_file_paths, desc="Parsing setfiles"):
    setfiles.append(SetFile(path=setfile_path,
                    files=set_to_path_list(args.svcomp_root, setfile_path)))

current_files = set(f for sf in setfiles for f in sf.files)
print(f"Found {len(current_files)} files")

sampled_sets = []
for setfile in tqdm(setfiles, desc="Sampling"):
    sampled_sets.append(sample_setfile(setfile, args.factor))
current_files = set(f for sf in sampled_sets for f in sf.files)
print(f"Sampled down to {len(current_files)} files")

dump_output(args.svcomp_root, args.output, sampled_sets, args.skip_copy)
# print(sampled_sets)
