# --- Day 17: Set and Forget ---

library(tidyverse)
library(gtools)
source("intcode.R")

options(digits = 22, scipen=999)

dt <- read_lines("day17.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# --- Part One ---

# create the intcode computer and run it
ic <- intcode_create(dt) %>% intcode_run()

# get the output, convert each of the outputs to a character, then collapse the
# output into a single string
out <- intcode_output(ic) %>%
  map_chr(chr) %>%
  paste(collapse = "") %>%
  # there are some trailing characters, get rid of them
  str_trim()

# show the output
cat(out, "\n")

# create a matrix of the scaffolding by splitting the output string into lines,
# then splitting each line into individual characters. if the character is a
# "#" then output a 1, otherwise a 0.
m <- str_split(out, "\n")[[1]] %>%
  map(~(str_split(.x, "")[[1]] == "#")*1) %>%
  # bind the rows into a matrix
  reduce(rbind)

# keep track of how many intersections there are
part_one <- 0
# loop through each row, and each column: however we can skip the left/right and
# top/bottom column/rows: intersections cannot occur in these positions.
for (i in 2:(nrow(m)-1)) {
  for (j in 2:(ncol(m)-1)) {
    # our matrix stores scaffolding as 1's and everything else as 0. if we are
    # at an intesection point, then above, below, left and right are all also a
    # 1. if we multiply these together we get 1. if _any_ are 0 then the whole
    # product will be 0.
    p <- (i-1) * (j-1)
    t <- m[i, j] * m[i-1, j] * m[i+1, j] * m[i, j-1] * m[i, j+1]
    part_one <- part_one + t * p
  }
}

part_one

# --- Part Two ---

dt[[1]] <- 2

get_path_str <- function() {
  m <- str_split(out, "\n")[[1]] %>%
    map(~c("." = 0,
           "#" = 1,
           "^" = 2,
           "V" = 3,
           "<" = 4,
           "> = 5")[str_split(.x, "")[[1]]] %>%
          as.numeric()) %>%
    # bind the rows into a matrix
    reduce(rbind)
  
  m <- unname(m)
  
  m <- cbind(rep(0, nrow(m)), m)
  m <- cbind(m, m[,1])
  m <- rbind(rep(0, ncol(m)), m)
  m <- rbind(m, m[1,])
  
  p <- c((1:nrow(m))[apply(m, 1, function(x) any(x >= 2))],
         (1:ncol(m))[apply(m, 2, function(x) any(x >= 2))])
  
  # start direction
  d <- switch(m[p[[1]], p[[2]]],
              `1` = c( 0,  0),
              `2` = c(-1,  0),
              `3` = c( 1,  0),
              `4` = c( 0, -1),
              `5` = c( 0,  1))
  
  s <- character(0)
  mv <- 0
  repeat {
    # can we move in the current direction?
    np <- p + d
    if (m[np[[1]], np[[2]]] == 1) {
      mv <- mv + 1
      p <- np
      next
    }
    
    if (mv > 0) {
      s <- c(s, as.character(mv+1))
    }
    mv <- 0
    if (abs(d[[1]]) == 1) {
      nd <- c(0, 1)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        s <- c(s, ifelse(d[[1]] == 1, "L", "R"))
        d <- nd
        p <- np
        next
      }
     
      nd <- c(0, -1) 
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        s <- c(s, ifelse(d[[1]] == 1, "R", "L"))
        d <- nd
        p <- np
        next
      }
    } else {
      nd <- c(1, 0)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        s <- c(s, ifelse(d[[2]] == 1, "R", "L"))
        d <- nd
        p <- np
        next
      }
      
      nd <- c(-1, 0)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        s <- c(s, ifelse(d[[2]] == 1, "L", "R"))
        d <- nd
        p <- np
        next
      }
    }
  
    break
  }
  s
}

# found this function implemented in Python on the Advent of Code subreddit.
# reimplemented in R
compress <- function(path) {
  # recursive function that takes as input the remaining path to be explored,
  # what paths have been assigned to what function, and what functions remain
  # to be assigned
  compress_fn <- function(path,
                          assigned = list(),
                          c_remain = c('A', 'B', 'C')) {
    # if the path is an empty vector, then return the path and assigned values
    if (length(path) == 0) return (list(path, assigned))
    
    seq <- NULL
    best_assigned <- list()
    
    # iterate through the path: start with a path of lenght 1, and iterate to
    # a path of length at maximum 9 (so a string of length 10)
    for (i in 1:(min(length(path), 9))) {
      # create a string by combining the elements of the path with a ","
      # seperating each element
      test_string <- paste(path[1:i], collapse = ",")
      
      # if the string is longer than 20 characters then it can't be used as a
      # function, so break
      if (str_length(test_string) >= 20) break()
      
      # take a copy of the current assigned list
      assigned_iter <- assigned
      # check: has this string already been assigned?
      if (is.null(assigned[[test_string]])) {
        # it hasn't
        # if there is nothing left to be assigned, then move to the next item in
        # the loop
        if (length(c_remain) == 0) next()
        # otherwise, set this test_string to be the first remaining function in
        # c_remain
        assigned_iter[[test_string]] <- c_remain[[1]]
        # call this function again, but exclude the values from path used so far
        # and the value from c_remain we just used
        rest <- compress_fn(path[-(1:i)], assigned_iter, c_remain[-1])
      } else {
        # call this function again, but exclude the values from path used so far
        rest <- compress_fn(path[-(1:i)], assigned, c_remain)
      }
      
      # if the first item in rest is not null
      if (!is.null(rest[[1]])) {
        # check to see if the first item in rest is longer than the current
        # value for seq
        if (is.null(seq) | length(rest[[1]]) < length(seq) - 1) {
          # it is, so update seq
          seq <- c(assigned_iter[[test_string]], rest[[1]])
          # replace best_assigend with assigned_iter
          best_assigned <- assigned_iter
          # add all of the values from the second item in rest to best_assigned
          for (k in names(rest[[2]])) {
            best_assigned[[k]] <- rest[[2]][[k]]
          }
        }
      }
    }
    # return our best result so far
    return (list(seq, best_assigned))
  }
  
  # iteratively call the path compression function, then return the results in
  # a more useful way where each function (main, A, B, C) is listed
  x <- compress_fn(path)
  res <- list(main = x[[1]])
  
  for (k in names(x[[2]])) {
    res[[ x[[2]][[k]] ]] <- k
  }
  
  res
}

input <- c(compress(get_path_str()), "n\n") %>%
  map(paste, collapse = ",") %>%
  paste(collapse = "\n")

ic <- dt %>%
  intcode_create() %>%
  intcode_add_ascii(input) %>%
  intcode_run()

# get the maximum output value, this should be > 127 and it should be the
# last value
ic %>%
  intcode_output() %>%
  max()
