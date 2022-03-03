import pandas as pd
import sys
import os
from pathlib import Path

results_dir = sys.argv[1]
filename = "results.csv"

frames = []
for process_dir in os.listdir(results_dir):
    path = os.path.join(process_dir, filename)
    if os.path.exists(path):
        t = pd.read_csv(path, index_col=0)
        frames.append(t)

df = pd.concat(frames)
df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
print(df)
df.to_csv('total_results.csv')
