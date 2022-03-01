import subprocess
import os
import sys
from pydriller import Repository
from datetime import datetime
import shutil

# runs the incremental_smallcommits.py script in an parallel mode (for faster benchmarking on the test server)
# the directory 'result' in the cwd will be overwritten!

# Usage: python3 parallel_benchmarking.py <number of processes>]

wd = os.getcwd()
res_dir = os.path.join(wd, 'result')
if os.path.exists(res_dir):
    shutil.rmtree(res_dir)
os.mkdir(res_dir)
os.chdir(res_dir)

num = sys.argv[1]
processes = []
url = 'https://github.com/facebook/zstd'
repo_name = 'zstd'
conf = "big-benchmarks"
build_script = 'build_compdb_zstd.sh'
full_path_analyzer = '/home/sarah/code/analyzer'
begin = datetime(2021,1,1)

# calculate number of interesting commits
i = sum(1 for _ in Repository(url, since=begin, only_no_merge=True).traverse_commits())
print("Number of potentially interesting commits:", i)
perprocess = i / num

for i in range(num):
    f = open("process" + str(i) + ".log", "a+")
    dir = "process1" + str(i)
    os.mkdir(dir)
    os.chdir(dir)
    # run script
    p = subprocess.Popen(['python3', os.path.join(full_path_analyzer, 'scripts', 'incremental_smallcommits.py'), full_path_analyzer, url, repo_name, build_script, conf, datetime.strftime(begin, '%Y/%m/%d'), str(perprocess * i), str(perprocess * (i + 1))], stdout=f)
    processes.append((p, f))
    os.chdir(res_dir)

for p, f in processes:
    p.wait()
    f.seek(0)
    print(f.read())
    f.close()
