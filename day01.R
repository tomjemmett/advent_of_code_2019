# --- Day 1: The Tyranny of the Rocket Equation ---

library(tidyverse)

# --- Part One ---

# read in the input, but convert the data to numeric's
dt <- read_lines("day01.txt") %>%
  as.numeric()

# create a function that will calculate the required fuel
# if the required fuel is negatige, as would be the case when x < 5, return 0
fuel_required <- function(x) max(floor(x/3)-2, 0)

# execute the function for each line of the inputs, and sum the results
dt %>% map_dbl(fuel_required) %>% sum()
# we have to use a map function here, because our function is not vectorised*.
# using map_dbl will execute the function for each of the inputs, return a
# numeric vector, which we can then simply pass into the sum function.

# (*): it sort of is, but because of the max argument it's only ever going to
#      return the maximum of all |_ x / 3 _| - 2, e.g. the greatest value of x.
#      as an alternative, we could use:
fuel_required <- function(x) pmax(floor(x / 3) - 2, 0)
sum(fuel_required(dt))

# --- Part Two ---

# we now need a function that will take an intial mass, caclulate the required
# fuel, then calculate the required fuel for the previous required fuel amount
# and iterate until we require no more fuel
fnx <- function(x) {
  # build a list of results for each step, set the first result to the weight
  # of the module
  res <- list()
  res[[1]] <- x
  
  # iterate while the weight of the fuel is > 0
  i <- 1
  while(TRUE) {
    # calculate the weight of the fuel required
    mn <- fuel_required(res[[i]])
    # weight of the fuel is 0, so stop the loop
    if (mn <= 0) break
    
    res[[i+1]] <- mn
    i <- i + 1
  }
  
  # we don't want to include the initial value which was the weight of the
  # module: only the weight of the fuel. Alternative would be
  #   reduce(res, sum) - x
  reduce(res[2:i], sum)
}

# as above, use map_dbl because our function isn't vectorised
dt %>% map_dbl(fnx) %>% sum()

