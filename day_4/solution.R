### PURPOSE OF THIS SCRIPT
## Solve day 4 of AOC 2024 in R


# Load dependencies ------------------------------------------------------------

library(dplyr)
library(stringr)


# Solve ------------------------------------------------------------------------


##### Functions #####

next_letter <- function(letter){

  if(letter == 'X'){
    return('M')
  }else if(letter == 'M'){
    return('A')
  }else{
    return('S')
  }

}

xmas_search <- function(ws, start_row, start_column,
                        letter = 'M'){

  row_min <- pmax(start_row - 1, 1)
  row_max <- pmin(start_row + 1, nrow(ws))

  col_min <- pmax(start_column - 1, 1)
  col_max <- pmin(start_column + 1, ncol(ws))


  xmas_count <- 0
  for(r in row_min:row_max){
    for(c in col_min:col_max){

      if(ws[r, c] == letter){

        if(letter == 'S'){

          return(1)

        }else{

          xmas_count <- xmas_count + xmas_search(ws, r, c, letter = next_letter(letter))

        }

      }

    }
  }

  return(xmas_count)

}


##### Script #####

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_4/input.txt"

lines <- readLines(filePath)

# Put into 2D matrix
max_line_length <- max(nchar(lines))
lines_matrix <- matrix("", nrow = length(lines), ncol = max_line_length)

for (i in seq_along(lines)) {
  line_chars <- strsplit(lines[i], "")[[1]]
  lines_matrix[i, seq_along(line_chars)] <- line_chars
}

# Search
total_xmas_count <- 0
for(i in 1:nrow(lines_matrix)){
  for(j in 1:ncol(lines_matrix)){

    if(lines_matrix[i,j] == "X"){
      total_xmas_count <- total_xmas_count + xmas_search(lines_matrix,
                                             i, j)
    }

  }
}

total_xmas_count
# Needs to be less than 24213

xmas_search(lines_matrix, 4, 130)


test_mat <- matrix(
  strsplit("MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX", "")[[1]],
  ncol = 10,
  nrow = 10
)

total_xmas_count <- 0
for(i in 1:nrow(test_mat)){
  for(j in 1:ncol(test_mat)){

    if(lines_matrix[i,j] == "X"){
      total_xmas_count <- total_xmas_count + xmas_search(test_mat,
                                                         i, j)
    }

  }
}

