import pandas as pd
import os

filename = "results.csv"

frames = []
for process_dir in os.listdir("."):
    path = os.path.join(process_dir, filename)
    if os.path.exists(path):
        t = pd.read_csv(path, index_col=0)
        frames.append(t)

df = pd.concat(frames)
df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
print(df)
df.to_csv('total_results.csv')
