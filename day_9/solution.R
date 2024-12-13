### PURPOSE OF THIS SCRIPT
## Solve day 9 of AoC 2024

library(stringr)

##### FUNCTIONS #####

recode <- function(map){

  map <- unlist(strsplit(map, ""))

  final_string <- c()

  for(i in 1:length(map)){

    if(i %% 2 == 0){

      final_string <- c(final_string, rep(-1, times = as.numeric(map[i])))

    }else{

      final_string <- c(final_string, rep(((i+1)/2) - 1, times = as.numeric(map[i])))

    }

  }

  return(final_string)

}

##### PARSE INPUT #####

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_9/input.txt"
#testPath <- "C:/Users/isaac/Documents/AoC/2024/day_9/test.txt"

input_lines <- readLines(filePath)
#test_lines <- readLines(testPath)


##### TEST PART 1 #####

recoded_input <- recode(input_lines)
