### PURPOSE OF THIS SCRIPT
## Solve day 1 of AoC 2024

# Load dependencies ------------------------------------------------------------

library(data.table)


# Solve ------------------------------------------------------------------------

### Part 1: calculate distance between lists

input <- fread("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt")

# Sort first column
setkey(input, V1)

V1_sorted <- data.table::copy(input$V1)

# Sort second column
setkey(input, V2)

V2_sorted <- data.table::copy(input$V2)

# Calculate distance
distance <- sum(abs(V2_sorted - V1_sorted))

distance

### Part 2: Calculate simularity score

left_values <- unique(input$V1)

similarity <- sum(sapply(left_values,
       function(x){
         sum(input$V2 == x)*x
       }))


