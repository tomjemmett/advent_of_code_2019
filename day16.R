# --- Day 16: Flawed Frequency Transmission ---

library(tidyverse)

options(digits = 22, scipen=999)

get_input <- function(i) {
  str_split(i, "")[[1]] %>% as.numeric()
}

dt <- get_input(read_lines("day16.txt"))

# --- Part One ---

ex1 <- get_input("12345678")
ex2 <- get_input("80871224585914546619083218645595")
ex3 <- get_input("19617804207202209144916044189917")
ex4 <- get_input("69317163492948606335995924319873")

# create an LxL square matrix from the base input
base_matrix <- function(L) {
  # the base input
  base <- c(0, 1, 0, -1)
  m <- matrix(0, L, L)
  for (i in 1:L) {
    b <- rep(base, each = i)
    for (j in 1:L) {
      k <- (j %% length(b)) + 1
      m[i, j] <- b[[k]]
    }
  }
  m
}

# function to run n phases against an input
# this uses a matrix from the base input, which we multiply the input vector at
# each phase by
part_one <- function(input, n) {
  # create the matrix for multiplication
  m <- base_matrix(length(input))
  
  # run n phases
  for (i in 1:n) {
    # multiply the matrix by the input, get rid of the sign (the absolute value)
    # and take get 10's position: update the input vector ready for the next
    # phase
    input <- abs(m %*% input)[,1] %% 10
  }
  
  # take the input vector, collapse it into a single string. only keep the first
  # 8 characters
  paste(input[1:8], collapse = "")
}

# verify that our function is running as expectede
stopifnot(all(
  part_one(ex1, 1) == "48226158",
  part_one(ex1, 2) == "34040438",
  part_one(ex1, 3) == "03415518",
  part_one(ex1, 4) == "01029498",

  part_one(ex2, 100) == "24176176",
  part_one(ex3, 100) == "73745418",
  part_one(ex4, 100) == "52432133"
))

part_one(dt, 100)

# --- Part Two ---

ex5 <- get_input("03036732577212944063491565474664")
ex6 <- get_input("02935109699940807407585447034323")
ex7 <- get_input("03081770884921959731165446850517")

# the solution for part one is very slow:
# let's inspect what the base matrix looks like for an 8x8

base_matrix(8)

# and a 10x10 matrix

base_matrix(10)

# the bottom right corner is an upper triangular matrix. that is, from the
# diagonal to the end it's all ones.
# so, if the offset puts us more than halfway through the list, then we just
# need to sum up from that element to the end for our solution

part_two <- function(input) {
  # get the offset
  offset <- as.numeric(paste(input[1:7], collapse = ""))
  
  stopifnot(offset >= length(input)/2)
  
  # repeat the input 10,000 times, and immediately take from the offset to the
  # end of the repeated input
  x <- rep(input, 10000)
  x <- x[(offset+1):length(x)]
  # get the length of the new vector
  L <- length(x)
  # 100 phases
  for (i in 1:100) {
    # print some progress
    cat("\r", str_pad(i, width = 3, side = "left"), "/", 100)
    # work from the very last value to the first: as we are progressively adding
    # all of the values (last, last+last-1, ...) this should be very quick
    s <- 0
    for (j in L:1) {
      # update the current sum
      s <- s + x[[j]]
      # update the current input's value, we don't need to access it again this
      # phase
      x[[j]] <- abs(s) %% 10
    }
  }
  # put a newline to the screen, then return the results
  cat("\n")
  paste(x[1:8], collapse = "")
}

# verify our solution works
stopifnot(all(
  part_two(ex5) == "84462026",
  part_two(ex6) == "78725270",
  part_two(ex7) == "53553731"
))

part_two(dt)

