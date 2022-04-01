import os
import sys
from pydriller import Repository
from datetime import datetime
import shutil
import psutil
import multiprocessing as mp
import incremental_pairsofcommits as exc

# runs the incremental_smallcommits.py script in an parallel mode (for faster benchmarking on the test server)
# the directory 'result' in the cwd will be overwritten!

# Usage: python3 parallel_benchmarking.py <number of processes>]

def runperprocess(processnum, full_path_analyzer, url, repo_name, build_script, conf, begin, start, end):
    psutil.Process().cpu_affinity([processnum * 2])
    exc.main(full_path_analyzer, url, repo_name, build_script, conf, begin, start, end)

if __name__ == '__main__':
  if len(sys.argv) != 3:
      print("Wrong number of parameters.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
      exit()

  wd = os.getcwd()
  res_dir = os.path.join(wd, 'result')
  if os.path.exists(res_dir):
      shutil.rmtree(res_dir)
  os.mkdir(res_dir)
  os.chdir(res_dir)

  full_path_analyzer = sys.argv[1]
  try:
      num = int(sys.argv[2])
  except ValueError:
      print("Parameter should be a number.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
      exit()
  numcores = psutil.cpu_count(logical=False)
  allowedcores = numcores - 2
  if num > allowedcores:
      print("Not enough physical cores on this maching (exist: ", numcores, " allowed: ", allowedcores, ")")
      exit()
  processes = []
  url = 'https://github.com/facebook/zstd'
  repo_name = 'zstd'
  conf = "big-benchmarks1"
  build_script = 'build_compdb_zstd.sh'
  begin = datetime(2021,2,1)

  # calculate number of interesting commits
  num_commits = sum(1 for _ in Repository(url, since=begin, only_no_merge=True).traverse_commits())
  print("Number of potentially interesting commits:", num_commits)
  perprocess = num_commits // num

  for i in range(num):
      dir = "process" + str(i)
      os.mkdir(dir)
      os.chdir(dir)
      # run script
      start = perprocess * i
      end = perprocess * (i + 1) if i < num - 1 else num_commits
      p = mp.Process(target=runperprocess, args=[i, full_path_analyzer, url, repo_name, build_script, conf, begin, start, end])
      p.start()
      processes.append(p)
      os.chdir(res_dir)

  for p in processes:
      p.join()
