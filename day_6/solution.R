### PURPOSE OF THIS SCRIPT
## Solve day 6 of AoC 2024


# Dive in ----------------------------------------------------------------------
library(dplyr)
library(furrr)
library(purrr)

##### FUNCTIONS #####

encode_position <- function(guard){

  if(guard == "v"){

    return(1)

  }else if(guard == "^"){

    return(2)

  }else if(guard == ">"){

    return(3)

  }else if(guard == "<"){

    return(4)

  }
}

in_bounds <- function(map, row, col){

  return(row >= 1 & row <= nrow(map) &
     col >= 1 & col <= ncol(map))

}

rotate_right <- function(map, row, col){

  if(map[row, col] == "v"){

    map[row, col] <- "<"

  }else if(map[row, col] == "^"){

    map[row, col] <- ">"

  }else if(map[row, col] == ">"){

    map[row, col] <- "v"

  }else if(map[row, col] == "<"){

    map[row, col] <- "^"

  }

  return(map)

}

take_step <- function(map, row, col){

  if(map[row, col] == "v"){

    proposed_new_row <- row + 1
    proposed_new_col <- col

  }else if(map[row, col] == "^"){

    proposed_new_row <- row - 1
    proposed_new_col <- col

  }else if(map[row, col] == ">"){

    proposed_new_row <- row
    proposed_new_col <- col + 1

  }else if(map[row, col] == "<"){

    proposed_new_row <- row
    proposed_new_col <- col - 1

  }


  if(!in_bounds(map, proposed_new_row, proposed_new_col)){

    map[row, col] = "."


  }else if(map[proposed_new_row, proposed_new_col] == "#"){

    return(take_step(rotate_right(map, row, col),
                     row, col))

  }else{

    map[proposed_new_row, proposed_new_col] = map[row, col]
    map[row, col] = "."


  }


  return(
    list(new_row = proposed_new_row,
         new_col = proposed_new_col,
         new_map = map)
  )

}

trace_path <- function(map, start_row, start_column){

  row <- start_row
  col <- start_column

  places_visited <- matrix(0,
                           nrow = nrow(map),
                           ncol = ncol(map))


  while(in_bounds(map, row, col)){

    encoded_current_pos <- encode_position(map[row, col])

    if(places_visited[row, col] == encoded_current_pos){

      return(list(
        places_visited = places_visited,
        cycle = TRUE
      ))

    }else{

      places_visited[row, col] <- encode_position(map[row, col])
      step <- take_step(map, row, col)
      row <- step$new_row
      col <- step$new_col
      map <- step$new_map

    }



  }

  return(list(places_visited = places_visited,
              cycle = FALSE))

}

make_map <- function(lines){

  str <- strsplit(lines, "") |> unlist()
  map <- matrix(str, nrow = length(lines),
                      ncol = nchar(lines[1]),
                      byrow = TRUE)

  return(map)

}

find_start <- function(map){

  for(i in 1:nrow(map)){
    for(j in 1:ncol(map)){
      if(map[i,j] %in% c("^", ">", "v", "<")){
        row_start <- i
        col_start <- j
        break
      }
    }
  }

  return(list(
    row_start = row_start,
    col_start = col_start
  ))

}


##### PARSE FILE #####

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_6/input.txt"
testPath <- "C:/Users/isaac/Documents/AoC/2024/day_6/test.txt"

input_lines <- readLines(filePath)
test_lines <- readLines(testPath)

input_map <- make_map(input_lines)
test_map <- make_map(test_lines)

##### TEST CASES #####

for(i in 1:nrow(test_map)){
  for(j in 1:ncol(test_map)){
    if(test_map[i,j] %in% c("^", ">", "v", "<")){
      row_start <- i
      col_start <- j
      break
    }
  }
}

visited <- trace_path(test_map, row_start, col_start)

##### PART 1 #####

starting_pos <- find_start(input_map)

visited <- trace_path(input_map, starting_pos$row_start, starting_pos$col_start)

sum(visited$places_visited > 0)


##### PART 2 #####

# starting_pos <- find_start(test_map)
#
# num_cycles <- 0
# for(i in 1:nrow(test_map)){
#   for(j in 1:ncol(test_map)){
#
#     if(test_map[i, j] == "."){
#
#       new_map <- test_map
#       new_map[i, j] <- "#"
#       visited <- trace_path(new_map, starting_pos$row_start, starting_pos$col_start)
#
#       num_cycles <- num_cycles + as.numeric(visited$cycle)
#
#
#     }
#
#   }
# }



starting_pos <- find_start(input_map)

num_cycles <- 0
possible_sites <- tibble()

visited <- visited$places_visited

for(i in 1:nrow(visited)){
  for(j in 1:ncol(visited)){

    if(visited[i, j] > 0 & !(i == starting_pos$row_start & j == starting_pos$col_start)){

      possible_sites <- bind_rows(possible_sites,
                                  tibble(
                                    row = i,
                                    col =j
                                  ))


    }

  }
}

# for(s in 1:nrow(possible_sites)){
#
#   new_map <- input_map
#   new_map[possible_sites$row[s], possible_sites$col[s]] <- "#"
#   visited <- trace_path(new_map, starting_pos$row_start, starting_pos$col_start)
#
#   num_cycles <- num_cycles + as.numeric(visited$cycle)
#
#   print(paste0((s*100) / nrow(possible_sites), "% done"))
#
# }


# Oof, this takes too much time
num_cycles

plan(multisession, workers = 8)
cycles <- future_map2(possible_sites$row, possible_sites$col,
           function(x, y){
             new_map <- input_map
             new_map[x, y] <- "#"
             visited <- trace_path(new_map, starting_pos$row_start, starting_pos$col_start)
             return(as.numeric(visited$cycle))
           },
           .progress = TRUE)

sum(cycles %>% unlist())
