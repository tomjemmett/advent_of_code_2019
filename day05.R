# --- Day 5: Sunny with a Chance of Asteroids ---

library(tidyverse)

# I want 1 based indexing to simplify the logic (the input is all 0 based)

"[.op_list" <- function(x, i) {
  x[[i + 1]]
}

"[<-.op_list" <- function(x, i=NULL, value) {
  x[[i + 1]] <- value
  x
}

"print.op_list" <- function(x, p = 0, n = 4) {
  tmp <- unclass(x)
  cat ("p: ", p, " n: ", n, "\n")
  print(tmp[(p+1):(p+n)])
}

intcode_computer <- function(op_list, input = 0) {
  p <- 0
  output <- 0
  
  while (op_list[p] != "99") {
    args <- list(
      ifelse(floor((op_list[p] %% 1000) / 100) == 1,
             op_list[p + 1],
             op_list[op_list[p + 1]]),
      ifelse(floor((op_list[p] %% 10000) / 1000) == 1 | op_list[p]%%100 == 4,
             op_list[p + 2],
             op_list[op_list[p + 2]])
    )
    
    instr <- op_list[p] %% 100
    
    if(instr == 1) {
      op_list[op_list[p + 3]] <- args[[1]] + args[[2]]
      p <- p + 4
    } else if(instr == 2) {
      op_list[op_list[p + 3]] <- args[[1]] * args[[2]]
      p <- p + 4
    } else if(instr == 3) {
      op_list[op_list[p + 3]] <- input
      p <- p + 2
    } else if(instr == 4) {
      output <- op_list[op_list[p + 1]]
      p <- p + 2
    } else if(instr == 5) {
      if (args[[1]] != 0) {
        p <- args[[2]]
      } else {
        p <- p + 3
      }
    } else if(instr == 6) {
      if (args[[1]] == 0) {
        p <- args[[2]]
      } else {
        p <- p + 3
      }
    } else if(instr == 7) {
      op_list[op_list[p + 3]] <- ifelse(args[[1]] < args[[2]], 1, 0)
      p <- p + 4
    } else if(instr == 8) {
      op_list[op_list[p + 3]] <- ifelse(args[[1]] == args[[2]], 1, 0)
      p <- p + 4
    } else {
      cat("p: ", p, "\n", "op", op_list[p], "\n")
      stop("error")
    }
  }
  return (output)
}

dt <- local({
  tmp <- read_file("day05.txt") %>%
    str_split(",", simplify = TRUE) %>%
    as.numeric()
  class(tmp) <- "op_list"
  tmp
})
dt

intcode_computer(dt, 1)
intcode_computer(dt, 5)
