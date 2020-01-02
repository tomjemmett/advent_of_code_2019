# --- Day 18: Many-Worlds Interpretation ---

library(sets) # load before tidyverse!
library(tidyverse)
library(datastructures)
library(memoise)
library(Rcpp)
library(compiler)
library(tictoc)

# warning: this is not quick! on my machine, part one takes ~ 4 minutes to run,
# part two takes ~7.5 minutes to run!

dt <- read_lines("day18.txt") %>%
  str_split("", simplify = TRUE)

# implements the reachable_keys function: this is just way, way too slow in R,
# so I've implemented it in C++
sourceCpp("day18.cpp")

# use cmpfun to try to eek out any performance improvements by JIT compiling
# maze should be a matrix of the input, pos should be a list for the starting
# positions for each of the robots
solve <- cmpfun(function(maze, pos) {
  # memoise the reachable_keys function (if we run this function twice with the
  # same input then it will return the previous results)
  reachable_keys_mf <- memoise(reachable_keys)
  
  # what are the keys that we need to find?
  all_keys <- lift_dl(set)(maze[maze %in% letters])
  
  # create a priority queue: we will visit nodes based on the distance (smaller
  # distances are visited first)
  q <- binomial_heap("numeric")
  # insert the initial positions into the queue
  insert(q, 0, list(d = 0, pos = pos, keys = set()))
  
  # what keys have been seen by each robot?
  # 1st list is for the robots
  # 2nd list is for the y position
  # 3rd list is for the x position
  # usage: seen[[p]][[y]][[x]]
  # the final value is a set of all of the keys seen at that position
  seen <- map(seq_along(pos),
              ~map(1:nrow(maze),
                   ~map(1:ncol(maze),
                        ~set())))
  # used for printing progress
  ctr <- 0
  max_keys <- 0
  
  # while we have items in the queue
  while (size(q) > 0) {
    # iterate the counter
    ctr <- ctr + 1
    
    # get the current lowest-priority item in the queue
    n <- pop(q)[[1]]
    
    # output the current counter position and how many keys have been found/the
    # current distance. show this every time a key is found
    if (max_keys < length(n$keys)) {
      max_keys <- length(n$keys)
      cat("\rctr:", str_pad(ctr, 5),
          " (keys: ", str_pad(max_keys, 2),
          ", d: ", str_pad(n$d, 4), ")\n",
          sep = "")
    } else if (!ctr %% 100) {
      # every 100 iterations outut the counter if we haven't found a key
      cat("\rctr:", str_pad(ctr, 5),
          sep = "")
    }
    
    # if we have found all the keys return the distance (as we are using a
    # minimum priority queue then the current distance is the minimum possible)
    if (n$keys == all_keys) return (n$d)
    
    # iterate over each of the robots
    for (p in seq_along(n$pos)) {
      # get the robots current position
      cx <- n$pos[[p]][[1]]
      cy <- n$pos[[p]][[2]]
      
      # get the current seen keys
      ks <- seen[[p]][[cy]][[cx]]
      # check to see if the keys that we have in n already exist for this robot:
      # in other words, check to make sure we haven't already been at this point
      # with this robot
      if (n$keys %e% ks) next()
      # mark this position for this robot with these keys as seen
      seen[[p]][[cy]][[cx]] <- ks | set(n$keys)
      
      # find the reachable keys from this position with the currently see keys
      rk <- reachable_keys_mf(maze, cx, cy, as.character(n$keys))
      
      # iterate over each of the reachable keys
      for (r in rk) {
        # update this robots position to be the new reachable key
        npos <- n$pos
        npos[[p]] <- c(r$x, r$y)
        # update the distance to reach this key
        nd <- n$d + r$l
        # insert this new position into the priority queue
        insert(q, nd, list(d    = nd,
                           pos  = npos,
                           keys = n$keys | r$c))
      }
    }
  }
  # if we don't find a solution, return NA
  return (as.numeric(NA))
})

# find @
sx <- (1:ncol(dt))[apply(dt, 2, function(x) any(x == "@"))]
sy <- (1:nrow(dt))[apply(dt, 1, function(x) any(x == "@"))]

# --- Part One ---
tic()
part_one <- solve(dt, list(c(sx, sy)))
toc()

# --- Part Two ---
dt2 <- dt
dt2[(sy-1):(sy+1), (sx-1):(sx+1)] <- c("@","#","@","#","#","#","@","#","@")
tic()
part_two <- solve(dt2, list(c(sx-1, sy-1),
                            c(sx+1, sy-1),
                            c(sx-1, sy+1),
                            c(sx+1, sy+1)))
toc()

cat("Part One: ", part_one, "\n",
    "Part Two: ", part_two, "\n")