### PURPOSE OF THIS SCRIPT
## Solve day 1 of AoC 2024

# Load dependencies ------------------------------------------------------------

library(data.table)
library(bench)
library(dplyr)
library(readr)

### Solution as a function
solve_day1 <- function(){

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

  right_counts <- input[,.N, by = V2]
  colnames(right_counts) <- c("V1", "N")

  setkey(right_counts, V1)
  setkey(input, V1)


  similarity <- input[right_counts, nomatch = NULL]

  similarity_score <- similarity[,.(score = sum(V1*N))]

  return(
    list(similarity = similarity_score,
         distance = distance)
  )

}


### base R version as function
baseR_solution <- function(){

  input <- read.table("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt",
                      header = FALSE)

  left_values <- sort(input[,1])
  right_values <- sort(input[,2])

  difference <- sum(abs(left_values - right_values))

  left_values <- unique(input[,1])

  similarity <- sum(sapply(left_values,
                           function(x){
                             sum(input[,2] == x)*x
                           }))

  return(
    list(similarity = similarity,
         distance = distance)
  )

}


### tidyverse solution as functoin
tidyverse_solution <- function(){

  # input <- read_delim("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt",
  #                     col_names = FALSE,
  #                     trim_ws = TRUE) %>%
  #   dplyr::select(-X2, -X3)
  input <- fread("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt") %>%
    as_tibble() %>%
    dplyr::rename(
      X1 = V1,
      X4 = V2
    )


  left_values <- input %>%
    dplyr::arrange(X1) %>%
    dplyr::select(X1) %>%
    unlist() %>%
    unname()


  right_values <- input %>%
    dplyr::arrange(X4) %>%
    dplyr::select(X4) %>%
    unlist() %>%
    unname()


  difference <- sum(abs(left_values - right_values))

  right_counts <- input %>%
    dplyr::count(X4) %>%
    dplyr::rename(
      X1 = X4
    )

  similarity_score <- input %>%
    dplyr::inner_join(right_counts,
                      by = "X1") %>%
    dplyr::summarise(
      score = sum(X1*n)
    ) %>%
    dplyr::select(score) %>%
    unlist() %>%
    unname()


  return(
    list(similarity = similarity_score,
         distance = distance)
  )

}

# Solve with data.table --------------------------------------------------------

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

right_counts <- input[,.N, by = V2]
colnames(right_counts) <- c("V1", "N")

setkey(right_counts, V1)
setkey(input, V1)


similarity <- input[right_counts, nomatch = NULL]

similarity_score <- similarity[,.(score = sum(V1*N))]
similarity_score


# Solve with base R only -------------------------------------------------------

input <- read.table("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt",
                    header = FALSE)

left_values <- sort(input[,1])
right_values <- sort(input[,2])

difference <- sum(abs(left_values - right_values))

left_values <- unique(input[,1])

similarity <- sum(sapply(left_values,
                         function(x){
                           sum(input[,2] == x)*x
                         }))


# Solve with tidyverse ---------------------------------------------------------

input <- read_delim("C:/Users/isaac/Documents/AoC/2024/day_1/input.txt",
                  col_names = FALSE,
                  trim_ws = TRUE) %>%
  dplyr::select(-X2, -X3)


left_values <- input %>%
  dplyr::arrange(X1) %>%
  dplyr::select(X1) %>%
  unlist() %>%
  unname()


right_values <- input %>%
  dplyr::arrange(X4) %>%
  dplyr::select(X4) %>%
  unlist() %>%
  unname()


difference <- sum(abs(left_values - right_values))

right_counts <- input %>%
  dplyr::count(X4) %>%
  dplyr::rename(
    X1 = X4
  )

similarity_score <- input %>%
  dplyr::inner_join(right_counts,
                    by = "X1") %>%
  dplyr::summarise(
    score = sum(X1*n)
  ) %>%
  dplyr::select(score) %>%
  unlist() %>%
  unname()




# Benchmark --------------------------------------------------------------------

(result <- bench::mark(
  solve_day1(),
  baseR_solution(),
  tidyverse_solution(),
  check = FALSE
))
# data.table: 2.6 ms
# baseR: 8.5 ms
# tidyverse: 74 ms!! Down to 13.6 if I swap in fread()
