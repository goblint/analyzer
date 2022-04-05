import os
import sys
from pydriller import Repository
from datetime import datetime
import shutil
import psutil
import multiprocessing as mp
import incremental_pairsofcommits as exc
import time
import random

# runs the incremental_smallcommits.py script in an parallel mode (for faster benchmarking on the test server)
# the directory 'result' in the cwd will be overwritten!

# Usage: python3 parallel_benchmarking.py <number of processes>]

def runperprocess(core, full_path_analyzer, url, repo_name, build_script, conf, begin, start, end):
    psutil.Process().cpu_affinity([core])
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
      numcores = int(sys.argv[2])
  except ValueError:
      print("Parameter should be a number.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
      exit()
  avail_phys_cores = psutil.cpu_count(logical=False)
  allowedcores = avail_phys_cores - 2
  if numcores > allowedcores:
      print("Not enough physical cores on this maching (exist: ", avail_phys_cores, " allowed: ", allowedcores, ")")
      exit()
  # This a list of cores to use. To get best results, choose a processes to core mapping, use only physical cores and have an equal number of processes per cache. The layout of physical/logical cores and sharing of caches is machine dependent. To find out use: 'lscpu --all --extended'.
  # for seidl19 test server:
  coremapping = [i for i in range(numcores - numcores//2)] + [i for i in range(avail_phys_cores//2, avail_phys_cores//2 + numcores//2)]
  processes = []
  url = 'https://github.com/facebook/zstd'
  repo_name = 'zstd'
  conf = "big-benchmarks1"
  build_script = 'build_compdb_zstd.sh'
  begin = datetime(2021,2,1)

  # calculate number of interesting commits
  num_commits = sum(1 for _ in Repository(url, since=begin, only_no_merge=True).traverse_commits())
  print("Number of potentially interesting commits:", num_commits)
  perprocess = num_commits // numcores

  for i in range(numcores):
      dir = "process" + str(i)
      os.mkdir(dir)
      os.chdir(dir)
      # run script
      start = perprocess * i
      end = perprocess * (i + 1) if i < numcores - 1 else num_commits
      p = mp.Process(target=runperprocess, args=[coremapping[i], full_path_analyzer, url, repo_name, build_script, conf, begin, start, end])
      p.start()
      processes.append(p)
      # time.sleep(random.randint(5,60)) # add random delay between process creation to try to reduce interference
      os.chdir(res_dir)

  for p in processes:
      p.join()
