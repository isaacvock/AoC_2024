### PURPOSE OF THIS SCRIPT
## Solve day 7 of AoC 2024

##### FUNCTIONS #####

vect <- c(2, 10, 23, 4)

result <- 50

### First pass
combos <- c(20, 22)

combos <- vect[1]
combos <- c(combos + vect[2], combos * vect[2])
c(combos + vect[3], combos*vect[3])

### Second pass
combos <- c(43, 45)


search_operations <- function(result, values,
                              part = 1){


  possibilities <- values[1]

  if(possibilities > result){
    return(FALSE)
  }

  for(i in 2:length(values)){

    if(part == 1){
      possibilities <- c(possibilities + values[i], possibilities * values[i])

    }else{

      possibilities <- c(possibilities + values[i], possibilities * values[i],
                         as.numeric(paste0(possibilities, values[i])))
    }

    possibilities <- possibilities[possibilities <= result]

    if(length(possibilities) == 0){
      return(FALSE)
    }

  }

  if(any(possibilities == result)){
    return(TRUE)
  }else{
    return(FALSE)
  }

}

find_max <- function(nums){

  result <- nums[1]
  operations <- rep("", times = length(nums) - 1)
  for(n in 2:length(nums)){

    if(nums[n] == 1){

      result <- result + 1
      operations[n-1] <- "+"

    }else{

      result <- result * nums[n]
      operations[n-1] <- "*"

    }

  }

  return(list(result = result,
              operations = operations))

}

find_operation <- function(nums, part = 1){

  nums <- as.numeric(nums)
  result <- nums[1]
  values <- nums[2:length(nums)]

  # max_possible <- find_max(values)
  #
  # if(max_possible$result < result){
  #
  #   return(FALSE)
  #
  # }
  #
  # if(max_possible$result == result){
  #
  #   return(TRUE)
  #
  # }

  search_result <- search_operations(result, values, part = part)

  return(search_result)



}


##### PARSE FILE #####

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_7/input.txt"
testPath <- "C:/Users/isaac/Documents/AoC/2024/day_7/test.txt"

input_lines <- readLines(filePath)
test_lines <- readLines(testPath)


##### TEST PART 1 #####

test_lines

nums <- strsplit(test_lines, " ")
nums <- lapply(nums,
               function(x){
                 return(gsub(":", "", x))
               })

can_do <- sapply(nums,
                 function(x) find_operation(x))

results <- sapply(nums,
                  function(x) as.numeric(x[1]))

sum(results[can_do])

##### ACTUALLY SOLVE PART 1 #####

nums <- strsplit(input_lines, " ")
nums <- lapply(nums,
               function(x){
                 return(gsub(":", "", x))
               })


can_do <- sapply(nums,
                 function(x) find_operation(x))

results <- sapply(nums,
                  function(x) as.numeric(x[1]))
answer <- sum(results[can_do])

sprintf("%14f", answer)
# 3311992226992 is too low
# 3312271365652 is right!


##### PART 2 #####
nums <- strsplit(input_lines, " ")
nums <- lapply(nums,
               function(x){
                 return(gsub(":", "", x))
               })


can_do <- sapply(nums,
                 function(x) find_operation(x, part = 2))

results <- sapply(nums,
                  function(x) as.numeric(x[1]))
answer <- sum(results[can_do])

sprintf("%14f", answer)
