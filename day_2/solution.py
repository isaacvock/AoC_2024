### PURPOSE OF THIS SCRIPT
## Implement solution to day 2 of AOC 2024 in Python

import numpy as np


# Determine safeness of array
def is_safe(A):

    monotonic = all(x < 0 for x in A) or all(x > 0 for x in A)
    stable = all(abs(x) <= 3 for x in A)

    if(monotonic and stable):
        return True
    else:
        return False
    

# Parse text file 
with open('day_2\\input.txt', 'r') as file:

    safe_count = 0
    
    for line in file:

        # Parse line to get array of numbers
        line_string = line.rstrip()  
        line_array = line.split()
        num_array = [int(x) for x in line_array]

        # Calculate difference array
        diff_array = np.diff(num_array)

        # Determine if array is safe
        safeness = is_safe(diff_array)

        safe_count += int(safeness)

