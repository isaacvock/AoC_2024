### PURPOSE OF THIS SCRIPT
## Solve day 8 of AoC 2024
library(dplyr)
library(MASS)


build_lookup <- function(map){

  database <- list()
  for(i in 1:nrow(map)){
    for(j in 1:ncol(map)){

      if(map[i, j] != "."){

        index <- as.character(map[i, j])

        database[[index]] = rbind(database[[index]],
                                          matrix(c(j, i), nrow = 1, ncol = 2))

      }

    }
  }

  return(database)

}


calculate_antinodes <- function(M,
                                 nrow_map,
                                 ncol_map){

  antinodes <- matrix(0, nrow = choose(nrow(M), 2)* 2,
                       ncol = 2)

  count <- 1
  for(i in 1:(nrow(M) - 1)){
    for(j in (i+1):nrow(M) ){


      xs <- c(M[i, 1], M[j, 1])
      ys <- c(-M[i, 2], -M[j, 2])

      # x-coordinates can be thought of as positive
      # and y-coordinates as negative, since larger y-coordinates (row indices)
      # go down the matrix and larger x-coordinates (col indices) go
      x1 <- min(xs)
      y1 <- max(ys[which(xs == min(xs))]) # Covers for edge case of x1 = x2

      x2 <- max(xs)
      y2 <- min(ys[which(xs == max(xs))])

      antinodes[count, 1] <- x1 - abs(x2 - x1)
      antinodes[count, 2] <- -(y1 - (y2 - y1))


      antinodes[count+1, 1] <- x2 + abs(x2 - x1)
      antinodes[count+1, 2] <- -(y2 + (y2 - y1))

      count <- count + 2

    }
  }

  # filter out those outside grid
  antinodes <- antinodes[dplyr::between(antinodes[,1], 1, ncol_map) &
                             dplyr::between(antinodes[,2], 1, nrow_map),]

  return(antinodes)
  # # Only return number of antinodes for now
  # return(nrow(antinodes))
}

calculate_antinodes2 <- function(M,
                                 nrow_map,
                                 ncol_map){

  antinodes <- matrix(c(0, 0), ncol = 2)
  count <- 1
  for(i in 1:(nrow(M) - 1)){
    for(j in (i+1):nrow(M) ){


      xs <- c(M[i, 1], M[j, 1])
      ys <- c(-M[i, 2], -M[j, 2])

      ys <- ys[order(xs)]
      xs <- xs[order(xs)]

      antinodes <- rbind(antinodes, matrix(c(xs, ys), nrow = 2, ncol = 2))

      rise <- diff(ys)
      run <- diff(xs)

      if(rise == 1 | run == 1){
        slope <- c(rise, run)
      }else{
        slope <- MASS::fractions(rise/run) %>%
          as.character() %>% strsplit(split = "/") %>%
          unlist() %>%
          as.numeric()
      }



      # Find first set of antinodes
      a_x <- xs[2] + slope[2]
      a_y <- ys[2] + slope[1]
      while(dplyr::between(a_x, 1, nrow_map) &
            dplyr::between(a_y, -ncol_map, -1)){

        antinodes <- rbind(antinodes, matrix(c(a_x, a_y), nrow = 1, ncol = 2))
        a_y <- a_y + slope[1]
        a_x <- a_x + slope[2]

      }

      # Find second set of antinodes
      a_x <- xs[1] - slope[2]
      a_y <- ys[1] - slope[1]
      while(dplyr::between(a_x, 1, nrow_map) &
            dplyr::between(a_y, -ncol_map, -1)){

        antinodes <- rbind(antinodes, matrix(c(a_x, a_y), nrow = 1, ncol = 2))
        a_y <- a_y - slope[1]
        a_x <- a_x - slope[2]

      }

      ### Might also need to add antinodes between antennas

    }
  }

  return(antinodes[2:nrow(antinodes),])

}


make_map <- function(lines){

  str <- strsplit(lines, "") |> unlist()
  map <- matrix(str, nrow = length(lines),
                ncol = nchar(lines[1]),
                byrow = TRUE)

  return(map)

}


##### PARSE FILE #####

filePath <- "C:/Users/isaac/Documents/AoC/2024/day_8/input.txt"
testPath <- "C:/Users/isaac/Documents/AoC/2024/day_8/test.txt"

input_lines <- readLines(filePath)
test_lines <- readLines(testPath)

input_map <- make_map(input_lines)
test_map <- make_map(test_lines)


##### TEST PART 1 #####

database <- build_lookup(test_map)


all_possible_As <- do.call(rbind, sapply(database, function(x) calculate_antinodes(x, nrow(test_map), ncol(test_map)))) %>%
  as_tibble()


all_possible_As %>%
  dplyr::distinct() %>%
  nrow()


all_possible_As <- sapply(database, function(x) calculate_antinodes2(x, nrow(test_map), ncol(test_map)))

do.call(rbind, all_possible_As) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct() %>%
  nrow()

##### PART 1 #####

database <- build_lookup(input_map)

all_possible_As <- sapply(database, function(x) calculate_antinodes(x, nrow(input_map), ncol(input_map)))

all_possible_As$`4`

do.call(rbind, all_possible_As) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct() %>%
  nrow()



##### PART 2 #####

database <- build_lookup(input_map)

all_possible_As <- sapply(database, function(x) calculate_antinodes2(x, nrow(input_map), ncol(input_map)))

do.call(rbind, all_possible_As) %>%
  dplyr::as_tibble() %>%
  dplyr::distinct() %>%
  nrow()

