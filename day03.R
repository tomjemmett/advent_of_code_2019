# --- Day 3: Crossed Wires ---
library(tidyverse)
library(hash)

# shamelessly stolen the idea for this from /u/jazende solution on Reddit.
# https://www.reddit.com/r/adventofcode/comments/e4axxe/2019_day_1_solutions/f9myhe6/?context=3

traverse_wire <- function(wire) {
  # experienced some weird issues if I didn't use an environment to capture the
  # variables, so stick everything that is modified in a new env.
  twe <- new.env()
  twe$x <- 0
  twe$y <- 0
  twe$count <- 0
  # hash table seems to perform better here than a standard list
  twe$wire_info <- hash()
  
  # a list of how each of the 4 direction's changes the x/y coordinates
  directions <- list(
    "R" = c( 1,  0),
    "L" = c(-1,  0),
    "U" = c( 0,  1),
    "D" = c( 0, -1)
  )
  
  # loop through each part of the wire
  for(part in wire) {
    # get the direction that we will be travelling in
    d <- directions[[str_sub(part, 1,  1)]]
    # and how far we will travel
    n <- str_sub(part, 2, -1) %>% as.numeric()
    
    # we move one step at a time, but we need to move n steps
    for (i in 1:n) {
      # update the current x/y value
      twe$x <- twe$x + d[[1]]
      twe$y <- twe$y + d[[2]]
      # update the counter
      twe$count <- twe$count + 1
      # and add the counters value to the current coordinate
      k <- paste0(twe$x, ",", twe$y)
      twe$wire_info[[k]] <- twe$count
    }
  }
  
  return(twe$wire_info)
}

# read in the file
dt <- read_lines("day03.txt") %>% str_split(",")

# process the two wires
wires <- map(dt, traverse_wire)

# the wires object has names that indicate the locations that wire visits. if
# the two wires cross then there will be a value at the crossing location in
# both of the wires objects.
crossings <- local({
  n1 <- names(wires[[1]])
  n2 <- names(wires[[2]])
  
  n1[n1 %in% n2]
})

# --- Part One ---
# for each of the crossings, convert the coordinate string (x,y) to two numbers
# (by splitting the string at each ","), then convert these number strings to
# numerics, take the absolute value's (make negatives positive) and sum the two
# coordinates values. Find the minimum of all of these values
closest <- map_dbl(crossings,
                   compose(sum, abs, as.numeric, str_split),
                   ",", simplify = TRUE) %>% min()
closest

# --- Part Two ---
# for each of the crossings, find how many steps it took to reach that point in
# each wire and add them up. find the minimum amount of steps required
steps <- map_dbl(crossings, ~wires[[1]][[.x]] + wires[[2]][[.x]]) %>% min()
steps
