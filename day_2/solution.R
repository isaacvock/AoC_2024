### PURPOSE OF THIS SCRIPT
## Solve day 2 of AOC 2024

# Load dependencies ------------------------------------------------------------

library(data.table)
library(dplyr)
library(readr)
library(stringr)


# Solve ------------------------------------------------------------------------

filepath <- "C:/Users/isaac/Documents/AoC/2024/day_2/input.txt"

con <- file(filepath, open = "r")

lines <- readLines(con)


# lines <- list(
#   "7 6 4 2 1",
#   "1 2 7 8 9",
#   "9 7 6 2 1",
#   "1 3 2 4 5",
#   "8 6 4 4 1",
#   "1 3 6 7 9",
#   "48 51 53 54 55 59",
#   "1 10 9 8 7 6"
# )



### Initial functions

is_monotonic <- function(v){

  len <- length(v)


  diffs <- rep(0, times = len - 1)

  decreasing <- v[2] < v[1]

  safe <- TRUE
  for(i in 2:(len - 1)){

    if(decreasing){
      safe <- (v[i] > v[i + 1]) & safe
    }else{
      safe <- (v[i] < v[i + 1]) & safe
    }


  }

  return(safe)

}

is_stable <- function(v){

  len <- length(v)

  diffs <- rep(0, times = len -1 )
  for(i in 1:(len - 1)){
    diffs[i] <- abs(v[i + 1] - v[i])
  }

  if(all(diffs <= 3) & all(diffs > 0)){
    return(TRUE)
  }else{
    return(FALSE)
  }

}


### Functions with safety buffers
is_monotonic2 <- function(v){

  len <- length(v)


  diffs <- rep(0, times = len - 1)

  decreasing <- v[2] < v[1]

  safe <- TRUE
  for(i in 2:(len - 1)){

    if(decreasing){
      safe <- (v[i] > v[i + 1]) & safe

    }else{
      safe <- (v[i] < v[i + 1]) & safe
    }

    if(!safe){
      break
    }


  }


  if(!safe){
    test1 <- is_monotonic(v[-i])
    test2 <- is_monotonic(v[-(i+1)])
    test3 <- is_monotonic(v[-(i-1)])

    safe <- test1 | test2 | test3

    trash_index <- c(i, i+1, i-1)[which(c(test1, test2, test3))]

    if(length(trash_index) == 0){
      trash_index <- NULL
    }

  }else{
    trash_index <- NULL
  }

  return(list(safe = safe,
              trash_index = trash_index))

}

is_stable2 <- function(v,
                       trash_index = NULL){

  if(!is.null(trash_index)){

    vs <- lapply(trash_index, function(x) v[-x])

    return(any(sapply(vs, function(x) is_stable(x) ) ))


  }else{

    len <- length(v)

    diffs <- rep(0, times = len -1 )
    safe <- TRUE
    for(i in 1:(len - 1)){
      diffs[i] <- abs(v[i + 1] - v[i])

      if(!all(diffs[1:i] <= 3) | !all(diffs[1:i] > 0)){
        safe <- FALSE
        break
      }


    }

    if(!safe){

      return(is_stable(v[-i]) | is_stable(v[-(i+1)]))

    }else{

      return(all(diffs <= 3) & all(diffs > 0))

    }

  }




}

safe_count <- 0
safe_bcount <- 0

for(l in lines){

  nums <- str_split(l, " ") %>%
    unlist() %>%
    as.integer()

  monotonic <- is_monotonic(nums)
  stable <- is_stable(nums)

  if(monotonic & stable){
    safe_count <- safe_count + 1
    safe_bcount <- safe_bcount + 1
  }else{


    monotonic_list <- is_monotonic2(nums)
    monotonic <- monotonic_list$safe
    ts <- monotonic_list$trash_index

    stable <- is_stable2(nums, trash_index = ts)

    if(monotonic & stable){
      safe_bcount <- safe_bcount + 1
    }else{

      print(nums)

    }

  }



}


close(con)


