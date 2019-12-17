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
  
  s <- ""
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
      s <- paste0(s, mv+1)
    }
    mv <- 0
    if (abs(d[[1]]) == 1) {
      nd <- c(0, 1)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        if (d[[1]] == 1) {
          s <- paste0(s, "L")
        } else {
          s <- paste0(s, "R")
        }
        d <- nd
        p <- np
        next
      }
     
      nd <- c(0, -1) 
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        if (d[[1]] == 1) {
          s <- paste0(s, "R")
        } else {
          s <- paste0(s, "L")
        }
        d <- nd
        p <- np
        next
      }
    } else {
      nd <- c(1, 0)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        if (d[[2]] == 1) {
          s <- paste0(s, "R")
        } else {
          s <- paste0(s, "L")
        }
        d <- nd
        p <- np
        next
      }
      
      nd <- c(-1, 0)
      np <- p + nd
      if (m[np[[1]], np[[2]]] == 1) {
        if (d[[2]] == 1) {
          s <- paste0(s, "L")
        } else {
          s <- paste0(s, "R")
        }
        d <- nd
        p <- np
        next
      }
    }
  
    break
  }
  s
}
s <- get_path_str()

str_split(s, "")[[1]] %>%
  paste0(collapse = ",") %>%
  str_replace_all("(\\d),(\\d)","\\1\\2") %>%
  str_split(",") %>%
  pluck(1) %>%
  compress()

a <- "R6L10R8R8"
b <- "R12L8L10"
c <- "R12L10R6L10"

main <- s %>%
  str_replace_all(a, "A") %>%
  str_replace_all(b, "B") %>%
  str_replace_all(c, "C")

input <- c(main,a,b,c,"n\n") %>%
  map_chr(str_replace_all, "([ABCAB])(?=[ABCAB])", "\\1,") %>%
  map_chr(str_replace_all, "([RL])(?=\\d)", "\\1,") %>%
  map_chr(str_replace_all, "(\\d)(?=[RL])", "\\1,") %>%
  paste(collapse = "\n")

input

ic <- intcode_create(dt) %>%
  intcode_run_to_input()

ic <- asc(input)[,1] %>%
  reduce(intcode_add_input, .init = ic) %>%
  intcode_run()

out <- intcode_output(ic)
out[length(out)]
