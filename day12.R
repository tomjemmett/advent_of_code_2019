# --- Day 12: The N-Body Problem ---

library(tidyverse)
library(pracma)

options(digits = 22)

print.nbodies <- function(data) {
  ndims <- ncol(data)/2
  
  pd <- partial(str_pad, width = 4, side = "left", pad = " ")
  
  p <- function(i, j) pd(data[i, j])
  v <- function(i, j) pd(data[i, j+ndims])
  
  for (i in 1:nrow(data)) {
    cat("pos=<x =", p(i,1), "y = ", p(i,2), "z = ", p(i,3),">, ")
    cat("vel=<x =", v(i,1), "y = ", v(i,2), "z = ", v(i,3),">\n")
  }
}

create_nbodies <- function(data) {
  nbodies <- data %>%
    str_extract_all("-?\\d+", simplify = TRUE) %>%
    apply(c(1,2), as.numeric)
  
  # add a duplicate set of columns for the velocities, which all start as 0
  nbodies <- cbind(nbodies, nbodies*0)
  
  class(nbodies) <- "nbodies"
  nbodies
}

# read in our data

dt <- read_lines("day12.txt") %>% create_nbodies()

# function to evolve a single dimension
evolve_dim <- function(moons, dim) {
  # calcualte the number of moons
  nmoons <- nrow(moons)
  # calculate the number of dimensions: because we represent as a matrix with
  # both the positions and velocities we need to halve the number of dimensions
  ndims <- ncol(moons)/2
  
  # get the position dimension
  pdim <- dim
  # get the velocity dimension
  vdim <- dim+ndims
  
  # iterate over every pair of moons
  for (i in 1:(nmoons-1)) {
    for (j in (i+1):nmoons) {
      # calculate the difference between moon i & j, and get the sign.
      # -1 if i < j, 0 if i == j, 1 if i > j
      c <- sign(moons[i, pdim] - moons[j, pdim])
      # update each moons velocity
      moons[i, vdim] <- moons[i, vdim] - c
      moons[j, vdim] <- moons[j, vdim] + c
    }
  }
  
  # now update the positions of each moon
  for (i in 1:nmoons) {
    moons[i, pdim] <- moons[i, pdim] + moons[i, vdim]
  }
  
  # return the moons
  return (moons)
}

# evolve all of the dimensions in the moons
evolve_moons <- function(moons) {
  # calculate the number of dimensions as above
  ndims <- ncol(moons)/2
  
  # iterate over each of the dimensions, update the moons
  for (i in 1:ndims) {
    moons <- evolve_dim(moons, i)
  }
  
  # return the fully updated moons
  return(moons)
}

# --- Part One ---
moons <- dt

# perform 1000 iterations
for (i in 1:1000) {
  moons <- evolve_moons(moons)
}

# calculate the energy
part_one <- sum(`*`(
  apply(moons[,1:3], 1, compose(sum, abs)),
  apply(moons[,4:6], 1, compose(sum, abs))
))

part_one

# --- Part Two ---

# This problem seems daunting at first, but the solution is to work out how long
# it takes each dimension to repeat. It will then be sufficient to work out the
# lowest common multiple of the current solution to part_two (which we set to 1
# initialy) and the time it takes that dimension to repeat

moons <- dt
part_two <- 1
# iterate over the dimensions
part_two <- map(1:3, function(x) {
  # get the initial positions of the moons in this dimension
  init <- moons[,x]
  # create a copy of the moons, and perform one evolution to start
  dx <- evolve_dim(moons, x)
  # count how many iterations are required
  i <- 1
  # stop iterating once we have reached the initial positions again
  while (!all(dx[,x] == init)) {
    # evolve the dimension
    dx <- evolve_dim(dx, x)
    # increase the counter
    i <- i + 1
  }
  # add one more to the total: the final state requires that the velocities all
  # return to 0 also
  i + 1
}) %>%
  # calculate the Lowest Common Multiple of all of the values: Lcm has to be
  # called for each pair of values, so use the reduce function to iterate each
  # successive pair
  reduce(Lcm)

part_two
