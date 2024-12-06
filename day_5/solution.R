### PURPOSE OF THIS SCRIPT
## Solve day 5 of 2024 AoC


# Get right into it ------------------------------------------------------------

library(stringr)
library(magrittr)
library(dplyr)

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_5/input.txt"

lines <- readLines(filePath)

breakpoint <- which(lines == "")

order_rules <- lines[1:(breakpoint - 1)]
potential_orders <- lines[(breakpoint + 1):length(lines)]

potential_orders <- str_split(potential_orders,
                              ",")

potential_orders <- lapply(potential_orders, as.numeric)


### What numbers are in the potential orders?
numbers <- potential_orders %>% unlist() %>% unique()

num_lookup <- tibble()

count <- 1
for(o in order_rules){

  nums <- str_split(o, "\\|") %>%
    unlist() %>%
    as.numeric()

  num_lookup <- bind_rows(
    num_lookup,
    tibble(query = nums[1],
           check = nums[2])
  )



}

is_proper <- function(nums, lookup){


  for(n in length(nums):2){

    nums_to_check <- lookup$check[lookup$query == nums[n]]

    if(any(nums_to_check %in% nums[1:(n-1)])){
      return(FALSE)
    }

  }

  return(TRUE)



}

get_middle <- function(nums, bool){

  return(nums[ceiling(length(nums) / 2)])

}


reorder_and_calc <- function(nums, lookup){

  while(!is_proper(nums, lookup)){

    pointer = length(nums)
    while(pointer > 1){

      nums_to_check <- lookup$check[lookup$query == nums[pointer]]


      flags <- nums[1:(pointer-1)] %in% nums_to_check

      if(any(flags)){

        bad_indices <- which(flags)

        val_to_move <- nums[min(bad_indices)]
        nums[min(bad_indices)] <- nums[pointer]
        nums[pointer] <- val_to_move

      }else{
        pointer <- pointer - 1
      }

    }

    for(n in length(nums):2){



    }

  }

  return(get_middle(nums))

}


proper_status <- sapply(potential_orders,
                        function(x) is_proper(x, num_lookup))

sum_track <- 0
for(ns in which(proper_status)){
  sum_track <- sum_track + get_middle(potential_orders[[ns]])
}

sum_track2 <- 0
for(ns in which(!proper_status)){
  sum_track2 <- sum_track2 + reorder_and_calc(potential_orders[[ns]], num_lookup)
}

sum_track
# 3014 too low

#### SANDBOX

order_test <- c("1|3", "3|2", "4|2", "1|4", "4|5")


num_lookup_test <- data.frame(number = c(1, 2, 3, 4, 5),
                         index = 1)

for(o in order_test){

  nums <- str_split(o, "\\|") %>%
    unlist() %>%
    as.numeric()

  index_1 <- num_lookup_test[num_lookup_test$number == nums[1],2]
  index_2 <- num_lookup_test[num_lookup_test$number == nums[2],2]

  if(index_2 <= index_1){
    num_lookup_test[num_lookup_test$number == nums[2],2] <- index_1 + 1
  }


}

num_lookup_test
