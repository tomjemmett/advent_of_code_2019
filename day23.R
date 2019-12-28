# --- Day 23: Category Six ---

library(tidyverse)
library(tictoc)

source("intcode.R")

options(digits = 22, scipen=999)

dt <- read_lines("day23.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

`[.zero_index` <- function(x, i) {
  x[[i+1]]
}

`[<-.zero_index` <- function(x, i, value) {
  x[[i+1]] <- value
  x
}

print.zero_index <- function(x) {
  for(i in seq_along(x)) {
    cat("[[", i-1, "]]\n", sep = "")
    print(x[[i]])
    cat("\n")
  }
}

# create 50 intcode computers
ics <- list()
class(ics) <- "zero_index"
for (i in 0:49) {
  ics[i] <- intcode_create(dt) %>%
    intcode_add_input(i) %>%
    intcode_run_to_input()
}

part_one <- function(ics) {
  repeat {
    for (i in 0:49) {
      if (length(ics[i]$input) == 0) {
        ics[i] <- intcode_add_input(ics[i], -1)
      }
      ics[i] <- intcode_run_to_input(ics[i])
      out <- intcode_output(ics[i])
      ics[i] <- intcode_clear_output(ics[i])
      
      if (length(out) == 0) next()
      # the computers output naddr, x, y values, so use a sequence length 3
      for (j in seq(1, length(out), 3)) {
        n <- out[[ j ]]
        x <- out[[j+1]]
        y <- out[[j+2]]
        
        if (n == 255) {
          return(y)
        }
        ics[n] <- intcode_add_input(ics[n], x, y)
      }
    }
  }
}

part_one(ics)

part_two <- function(ics) {
  nat <- list(x = 0, y = 0, ctr = 0)
  pre_nat <- Inf
  
  ctr <- 0
  repeat {
    ctr <- ctr + 1
    for (i in 0:49) {
      if (length(ics[i]$input) == 0) {
        ics[i] <- intcode_add_input(ics[i], -1)
      }
      ics[i] <- intcode_run_to_input(ics[i])
      out <- intcode_output(ics[i])
      ics[i] <- intcode_clear_output(ics[i])
      
      if (length(out) == 0) next()
      # the computers output naddr, x, y values, so use a sequence length 3
      for (j in seq(1, length(out), 3)) {
        n <- out[[ j ]]
        x <- out[[j+1]]
        y <- out[[j+2]]
        
        if (n == 255) {
          pre_nat <- nat$y
          nat$x <- x
          nat$y <- y
          nat$ctr <- ctr
        } else {
          ics[n] <- intcode_add_input(ics[n], x, y)
        }
      }
    }
    
    cat("->", str_pad(ctr, 3))
    if (all(map(ics, "input") %>% map_dbl(length) == 0)) {
      if (nat$ctr + 1 == ctr) {
        cat (" DONE\n")
        return (nat$y) 
      }
      cat(" NAT", nat$y)
      ics[0] <- intcode_add_input(ics[0], nat$x, nat$y)
    }
    cat("\n")
  }
}

part_two(ics)
