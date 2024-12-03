### PUPROSE OF THIS SCRIPT
## Implement solution to day 1 AoC 2024

import pandas as pd
import numpy as np

df = pd.read_csv("day_1\\input.txt", sep = "   ", header = None, engine = 'python')


left_values = df.iloc[:,0].tolist()
right_values = df.iloc[:,1].tolist()

left_values.sort()
right_values.sort()

# Calculate distance
distance = sum([abs(x - y) for x, y in zip(left_values, right_values)])


### Part 2: Calculate similarity score

right_counts = df.groupby(df.columns[1])[df.columns[0]].nunique().reset_index()


right_counts.columns = ['left', 'N']
df.columns = ['left', 'right']

dfcounts = pd.merge(df, right_counts, on = 'left', how = 'inner')

score = (dfcounts['left'] * dfcounts['N']).sum()