# --- Day 15: Oxygen System ---

library(tidyverse)
library(Matrix)
library(tictoc)

source("intcode.R")

options(digits = 22, scipen=999)

dt <- read_lines("day15.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# list of directions: how we move based on input
directions <- list(
  c(x =  0, y = -1), # north
  c(x =  0, y =  1), # south
  c(x = -1, y =  0), # west
  c(x =  1, y =  0)  # east
)

# --- Part One ---
ic <- intcode_create(dt)

# I originally had no idea how large a matrix was needed, so I set it to
# a very high number and figured out what the actual grid size needed was.
size <- 80
visited <- matrix(0, size, size)

init <- c(x = floor(size / 2), y = floor(size / 2))
visited[init[["y"]], init[["x"]]] <- 1

# start off going in every direction
queue <- list(
  list(d = 1, ic = ic, s = 0, p = init, h = numeric(0)),
  list(d = 2, ic = ic, s = 0, p = init, h = numeric(0)),
  list(d = 3, ic = ic, s = 0, p = init, h = numeric(0)),
  list(d = 4, ic = ic, s = 0, p = init, h = numeric(0))
)

min_x <- init[["x"]]
max_x <- min_x
min_y <- init[["y"]]
max_y <- min_y

moves <- 0
tic()
# while we still have items in the queue
while (length(queue) > 0) {
  # get the first item from the queue and remove it
  q <- queue[[1]]
  queue <- queue[-1]
  
  # adjust our position to where we have moved
  p <- q$p + directions[[q$d]]
  
  if (p[["x"]] < min_x) min_x <- p[["x"]]
  if (p[["x"]] > max_x) max_x <- p[["x"]]
  if (p[["y"]] < min_y) min_y <- p[["y"]]
  if (p[["y"]] > max_y) max_y <- p[["y"]]
  
  # ignore this cell if we have already tried it.
  # the trick of this algorithm is to find the quickest
  # way to the solution. If we have found a path here already, then we will
  # have reached this point via a shorter path, so we can ignore it
  if (visited[p[["y"]], p[["x"]]] != 0) {
    # we have already visited here
    next
  }
  # mark this as visited
  visited[p[["y"]], p[["x"]]] <- 1
  
  # run the intcode with this input until we get some output
  ic <- q$ic %>%
    intcode_add_input(q$d) %>%
    intcode_run(FALSE)
  
  # get the value of the output, and remove this output from the intcode
  # computer
  out <- intcode_output(ic)[[1]]
  ic <- intcode_clear_output(ic)
  
  # we have hit a dead end
  if (out == 0) {
    # mark as a dead end
    visited[p[["y"]], p[["x"]]] <- -1
    next
  }
  
  # add this direction to the history
  h <- c(q$h, q$d)
  # increase the number of moves made
  s <- q$s + 1
  moves <- moves + 1
  
  # we have found the oxygen system!
  if (out == 2) {
    part_one <- s
    visited[p[["y"]], p[["x"]]] <- 2
    oxygen <- p
  }
  
  # add the different directions that we can move to the end of the queue. we
  # can ignore one direction each time: there is no need to go back from where
  # we came from.
  if (q$d != 2) {
    queue[[length(queue)+1]] <- list(d = 1, ic = ic, s = s, p = p, h = h)
  }
  if (q$d != 1) {
    queue[[length(queue)+1]] <- list(d = 2, ic = ic, s = s, p = p, h = h)
  }
  if (q$d != 4) {
    queue[[length(queue)+1]] <- list(d = 3, ic = ic, s = s, p = p, h = h)
  }
  if (q$d != 3) {
    queue[[length(queue)+1]] <- list(d = 4, ic = ic, s = s, p = p, h = h)
  }
}
toc()

# --- Part Two ---

# take a copy of the visited matrix
vm <- visited

# start from the oxygen position, and loop over all of the possible cells that
# we can reach from that cell. keep looping until we can no longer move.
next_cells <- list(oxygen)
# keep track of how many iterations it takes
t <- 0
while(length(next_cells) > 0) {
  t <- t + 1
  # get the list of next cells to iterate over, call this current cells, then
  # create a new empty list for the next cells
  current_cells <- next_cells
  next_cells <- list()
  
  # loop through all of the current cells
  for (i in current_cells) {
    # set the cell to be 3: visited
    vm[i[["y"]], i[["x"]]] <- 3
    
    # loop over all of the possible directions we can move
    for (j in directions) {
      # create the new position
      p <- i + j
      # if the cell is a value of 1, then we haven't yet vistied this cell, so
      # add it to the list of next cells. any other value would indicate we have
      # either already been there, or it's a wall
      if (vm[p[["y"]], p[["x"]]] == 1) {
        next_cells[[length(next_cells)+1]] <- p
      }
    }
  }
}

# our time is one less than the calculated answer
part_two <- t - 1

# re-mark the oxygen location
vm[oxygen[["y"]], oxygen[["x"]]] <- 2
# show the board
vm[min_y:max_y, min_x:max_x] %>%
  apply(1, function(x) {
    case_when(x == -1 ~ "#",
              x ==  1 ~ " ",
              x ==  2 ~ "o",
              x ==  3 ~ ".",
              x ==  0 ~ "#") %>%
      paste0(collapse = "")
  }) %>%
  paste0(collapse = "\n") %>%
  cat("\n")

part_one
part_two
