# --- Day 18: Many-Worlds Interpretation ---

# I have rewrote my solution based using a Python solution from Reddit. This
# solves the problem in a reasonable time in R.
# https://www.reddit.com/r/adventofcode/comments/ec8090/2019_day_18_solutions/fbagnxw/

library(tidyverse)
library(datastructures)
library(compiler)

# read in the data as a matrix
dt <- read_lines("day18.txt") %>%
  str_split("", simplify = TRUE)

# I'm using compiler::cmpfun to try to eek out a bit more performance from R.

# function to calculate the distance from a given location to every other key or
# door that is reachable in the maze
distances_from <- cmpfun(function(maze, x, y) {
  q <- queue()
  # insert into the queue the initial position: it's route starts as an empty
  # character vector
  insert(q, list(list(x = x, y = y, dist = 0, route = character(0))))
  # hold the route info
  route_info <- list()
  # what directions can we move?
  dirs <- list(c(-1,0),c(1,0),c(0,-1),c(0,1))
  # while there are items in the queue
  while (size(q) > 0) {
    # remove the first item out of the queue
    n <- pop(q)[[1]]
    # what character is at this position?
    c <- maze[n$y, n$x]
    # if it's a letter or a door, and we have moved some distance
    if (c %in% c(letters, LETTERS) && n$dist > 0) {
      # then add this item to the route
      route_info[[c]] <- n[c("dist", "route")]
      # update the current route to include this
      n$route <- c(n$route, c)
    }
    # mark this place as visited
    maze[n$y, n$x] <- "#"
    # iterate over the areas we can move to
    for (d in dirs) {
      nx <- n$x + d[[1]]
      ny <- n$y + d[[2]]
      # if the new position isn't a #, then add this position to the queue
      if (maze[ny, nx] != "#") {
        insert(q, list(list(x = nx, y = ny, dist = n$dist+1, route = n$route)))
      }
    }
  }
  # return the route_info
  route_info
})
# find all of the options for movement in the maze from each named position to
# every other named position
find_routeinfo <- cmpfun(function(maze) {
  route_info <- list()
  # iterate over every cell in the matrix
  for (y in 1:nrow(maze)) {
    for (x in 1:ncol(maze)) {
      c <- maze[y, x]
      # if this cell is either a key or one of the start location, then work
      # out the distances to every other position
      if (c %in% c(letters,"1","2","3","4","@")) {
        route_info[[c]] <- distances_from(maze, x, y)
      }
    }
  }
  route_info
})

# function to solve the problem: takes in the route_info for the maze, an
# initial position, and the keys to find
solve <- cmpfun(function(route_info, info, keys) {
  # iterate once for every key
  for (i in seq_along(keys)) {
    # create a list for the next iteration
    nextinfo <- list()
    # iterate over all of the current items in info
    for (item in info) {
      # iterate over each of the keys
      for (k in keys) {
        # if this key has already been found then skip
        if (k %in% item$keys) next()
        
        # for each of the robots
        for (robot in seq_along(item$locs)) {
          # get the route info to this key
          ri <- route_info[[item$locs[[robot]]]][[k]]
          # if this robot cannot reach this key then move to the next robot
          if (is.null(ri)) next()
          # check to see if we can reach this key
          if (all(
            ri$route %in% item$keys |
            str_to_lower(ri$route) %in% item$keys
          )) {
            # we can reach the key, so update the info object
            nd <- item$dist + ri$dist
            # make sure to sort the keys we have found: this is to ensure that
            # if we reach key a then b, but also b then a, only the shortest
            # path is considered
            nk <- sort(c(item$keys, k), method = "shell")
            # update the location of this robot
            nl <- item$locs
            nl[[robot]] <- k
            # create a key for the info object by combining the robots positions
            # with the keys found
            j <- paste(c(nl, "|", nk), collapse = "")
            # check to see if this key exists in nextinfo: insert it if not,
            # update it only if the distance is shorter
            if (is.null(nextinfo[[j]]) || nd < nextinfo[[j]]$dist) {
              nextinfo[[j]] <- list(locs = nl,
                                    keys = nk,
                                    dist = nd)
            }
          }
        }
      }
    }
    # update the info object for the next iteration
    info <- nextinfo
  }
  # pull the "dist" values from the info object and return the minimum
  map_dbl(info, "dist") %>% min()
})

part_one <- function(maze) {
  # get the route info for this maze
  route_info <- find_routeinfo(maze)
  # find the keys
  keys <- names(route_info)[names(route_info) %in% letters]
  # build the initial info for this robot
  info <- list("@|" = list(locs = "@",
                           keys = character(0),
                           dist = 0))
  # solve
  solve(route_info, info, keys)
}

part_two <- function(maze) {
  # update the maze for part two: replace the @ with 1,2,3,4 for each of the
  # new start positions
  # first find the @
  start <- seq_along(maze)[maze == "@"] - 1
  sy <- start %% nrow(maze) + 1
  sx <- floor(start / nrow(maze)) + 1
  # then update the maze
  maze[(sy-1):(sy+1), (sx-1):(sx+1)] <- c("1","#","3","#","#","#","2","#","4")
  # get the route info for this maze
  route_info <- find_routeinfo(maze)
  # find the keys for this maze
  keys <- names(route_info)[names(route_info) %in% letters]
  # build the initial info object for these robots
  info <- list("1234|" = list(locs = c("1","2","3","4"),
                              keys = character(0),
                              dist = 0))
  # solve
  solve(route_info, info, keys)
}


part_one(dt)
part_two(dt)
