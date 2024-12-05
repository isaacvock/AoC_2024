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

xmas_search_simple <- function(ws, start_row, start_column){


  row_min <- pmax(start_row - 1, 1)
  row_max <- pmin(start_row + 1, nrow(ws))

  col_min <- pmax(start_column - 1, 1)
  col_max <- pmin(start_column + 1, ncol(ws))


  xmas_count <- 0

  for(rdiff in -1:1){

    if(rdiff == 0){
      cvect <- c(-1, 1)
    }else{
      cvect <- -1:1
    }

    for(cdiff in cvect){

      row_bound <- start_row + rdiff*3
      col_bound <- start_column + cdiff*3


      if(row_bound >= 1 & row_bound <= nrow(ws) &
         col_bound >= 1 & col_bound <= ncol(ws)){

        indices <- data.frame(
          row = seq(from = start_row, to = row_bound, length.out = 4),
          col = seq(from = start_column, to = col_bound, length.out = 4)
        )

        word_query <- paste(ws[cbind(indices$row, indices$col)], collapse="")
        if(word_query == "XMAS"){
          xmas_count <- xmas_count + 1
        }

      }

    }
  }

  return(xmas_count)

}

mas_x_search <- function(ws, start_row, start_column){


  if(start_row > 1 & start_row < nrow(ws) &
     start_column > 1 & start_column < nrow(ws) ){


    X1_diag_row1 <- start_row - 1
    X1_diag_col1 <- start_column - 1
    X1_diag_row2 <- start_row + 1
    X1_diag_col2 <- start_column + 1

    X2_diag_row1 <- start_row - 1
    X2_diag_col1 <- start_column + 1
    X2_diag_row2 <- start_row + 1
    X2_diag_col2 <- start_column - 1

    Q1 <- paste0(ws[X1_diag_row1, X1_diag_col1],
            ws[X1_diag_row2, X1_diag_col2])

    Q2 <- paste0(ws[X2_diag_row1, X2_diag_col1],
            ws[X2_diag_row2, X2_diag_col2])




  }else{

    Q1 <- "NO"
    Q2 <- "NO"


  }


  if((Q1 %in% c("SM", "MS")) & (Q2 %in% c("SM", "MS"))){
    return(1)
  }else{
    return(0)
  }





}

xmas_search_exhaustive <- function(ws, start_row, start_column,
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


##### PART 1 #####

test_mat <- matrix(
  strsplit("MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX", "")[[1]],
  ncol = 10,
  nrow = 10,
  byrow = TRUE
)

total_xmas_count <- 0
total_masX_count <- 0
for(i in 1:nrow(test_mat)){
  for(j in 1:ncol(test_mat)){

    if(test_mat[i,j] == "X"){
      total_xmas_count <- total_xmas_count + xmas_search_simple(test_mat,
                                                         i, j)
    }

    if(test_mat[i,j] == "A"){
      total_masX_count <- total_masX_count + mas_x_search(test_mat, i, j)
    }

  }
}

total_xmas_count
total_masX_count

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
      total_xmas_count <- total_xmas_count + xmas_search_simple(lines_matrix,
                                             i, j)
    }

  }
}

total_xmas_count


# Search
total_masX_count <- 0
for(i in 1:(nrow(lines_matrix))){
  for(j in 1:(ncol(lines_matrix))){

    if(lines_matrix[i,j] == "A"){
      total_masX_count <- total_masX_count + mas_x_search(lines_matrix,
                                                                i, j)
    }

  }
}

total_masX_count

#
#
# xmas_search(lines_matrix, 4, 130)
#
#
# test_mat <- matrix(
#   strsplit("MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX", "")[[1]],
#   ncol = 10,
#   nrow = 10
# )
#
# total_xmas_count <- 0
# for(i in 1:nrow(test_mat)){
#   for(j in 1:ncol(test_mat)){
#
#     if(lines_matrix[i,j] == "X"){
#       total_xmas_count <- total_xmas_count + xmas_search(test_mat,
#                                                          i, j)
#     }
#
#   }
# }
#
#
#
# # Example 2D matrix
# mat <- matrix(1:12, nrow = 3, byrow = TRUE)
# print(mat)
# #      [,1] [,2] [,3] [,4]
# # [1,]    1    2    3    4
# # [2,]    5    6    7    8
# # [3,]    9   10   11   12
#
# # Data frame with row and column indices
# indices <- data.frame(
#   row = c(1, 2, 3, 2),
#   col = c(3, 2, 4, 1)
# )
# print(indices)
# #   row col
# # 1   1   3
# # 2   2   2
# # 3   3   4
# # 4   2   1
#
# # Extract values from the matrix
# values <- mat[cbind(indices$row, indices$col)]
# print(values)
# # [1]  3  6 12  5
#
#
