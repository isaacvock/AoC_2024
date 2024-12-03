### PURPOSE OF THIS SCRIPT
## Solve day 3 of AOC 2024 in R


# Load dependencies ------------------------------------------------------------

library(data.table)
library(dplyr)
library(readr)
library(stringr)


# Solve ------------------------------------------------------------------------

##### PART 1 #####
filepath <- "C:/Users/isaac/Documents/AoC/2024/day_3/input.txt"
mem <- readChar(filepath, file.info(filepath)$size)

multiplications <- str_extract_all(mem, "mul\\(\\d{1,3},\\d{1,3}\\)") %>%
  unlist()

pattern <- "mem\\(\\d{1,3},\\d{1,3}\\)"



parse_mul <- function(str){

  str <- gsub("mul\\(", "", str)
  str <- gsub("\\)", "", str)
  nums <- str_split(str, ",") %>%
    unlist() %>%
    as.integer()

  return(prod(nums))

}

sum(sapply(
  multiplications,
  function(x) parse_mul(x)
))

##### PART 2 #####

filepath <- "C:/Users/isaac/Documents/AoC/2024/day_3/input.txt"
mem <- readChar(filepath, file.info(filepath)$size)

statements <- str_split(mem, "(?=don't\\(\\)|do\\(\\))") %>%
  unlist()

total_sum <- 0
for(i in 1:length(statements)){

  if(i == 1){

    multiplications <- str_extract_all(statements[i], "mul\\(\\d{1,3},\\d{1,3}\\)") %>%
      unlist()

    total_sum <- total_sum + sum(sapply(
      multiplications,
      function(x) parse_mul(x)
    ))

  }else{

    if(str_starts(statements[i], "do\\(\\)")){

      multiplications <- str_extract_all(statements[i], "mul\\(\\d{1,3},\\d{1,3}\\)") %>%
        unlist()

      total_sum <- total_sum + sum(sapply(
        multiplications,
        function(x) parse_mul(x)
      ))

    }

  }

}
