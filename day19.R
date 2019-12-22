# --- Day 19: Tractor Beam ---

library(tidyverse)
library(tictoc)

source("intcode.R")

options(digits = 22, scipen=999)

dt <- read_lines("day19.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()


ic <- intcode_create(dt)

ic_run <- function(x, y) {
  ic %>%
    intcode_add_input(x, y) %>%
    # run until we get the first output value
    intcode_run(FALSE) %>%
    intcode_output()
}

# --- Part One ---

part_one <- function() {
  # keep track of the start and end point that are affected by the tractor beam
  start <- 0
  end <- 0
  
  # keep track of how many points on each row that are affected by the tractor
  # beam
  output <- numeric(50)
  
  i <- 0
  
  print_current_status <- function() {
    # print information for each row, showing:
    # * the current row that we are on
    # * the output for this row where:
    #   - a . if the tractor beam doesn't affect this point
    #   - a # if the tractor beam does affect this point
    # * the number of points that the tractor beam affects on this row
    # * the start position of the points that are affected
    # * the end position of the points that are affected
    cat(str_pad(i, 2),
        ": ",
        rep('.', start),
        rep('#', output[[i+1]]),
        rep('.',50-end-1),
        #ifelse(end == 0 && output[[i+1]] == 0, ".", ""),
        " > ",
        output[[i+1]],
        ", ",
        str_pad(start, 2),
        ", ",
        str_pad(end, 2),
        "\n", sep="")
  }

  # the first 10 rows aren't always contiguous, so start from the far left (x=0)
  # and iterate to the right (x=9). Keep track of where we find the first point
  # that is affected, and iterate until we find the first cell that isn't
  # affected.
  for (i in 0:9) {
    # loop over the first 10 cells
    # keep a track of the first cell affected
    s <- 0
    for (j in 0:9) {
      o <- ic_run(j, i)
      # if this is the first time we have seen a 1 on this row, then set the
      # start position
      if (o == 1 && s == 0) s <- j
      # if we have already found an affected point, but then we move to a cell
      # that isn't affected, then the rest of the points to the right will also
      # not be affected, so we can abort calculating the rest
      if (s > 0 && o == 0) break()
      
      # update the output for this row
      output[[i+1]] <- output[[i+1]] + o
    }
    # set the start position for this row
    start <- s
    # set the end position for this row. If we find no affected points then we
    # will have an end point equal to -1
    end <- start + output[[i+1]]-1
    # show the output for this row
    print_current_status()
  }
  
  # now iterate over the rest of the rows. This time we will begin from the
  # start cell and assume that everything to the left is 0. From the starting
  # position we move right until we find an affected point. We then continue
  # from the end point from the previous row and iterate that until we find a
  # non-affected point. This assumes that everything between start and end
  # are affected, and everything else is not
  for (i in 10:49) {
    # find new start position
    repeat {
      o <- ic_run(start, i)
      # if the output is 1, we have found the first affected point
      if (o == 1) break
      # otherwise, increase the starting point and loop
      start <- start + 1
    }
    # find new end position
    repeat {
      o <- ic_run(end, i)
      # if the output is 0 then we have moved past the affected points, so
      # decrease the end point counter and exit the loop
      if (o == 0) {
        end <- end - 1
        break
      }
      # otherwise, increase the ending point and loop
      end <- end + 1
    }
    # store the number of affected points for this row
    output[[i+1]] <- end - start + 1
    
    # print the current output
    print_current_status()
  }
  
  # return the sum of the affected points for each row
  sum(output)
}

part_one()

# --- Part Two ---

part_two <- function() {
  # the first point that we should check is 0,99: this would be the first point
  # a 100x100 ship could fit
  x <- 0
  y <- 99
  
  cat("\r")
  # if the ship is to fit in a 100x100 grid on this row, then it will fit in the
  # left most point on that row. Therefore, if we find that the ship doesn't fit
  # on the first point, then we can simply skip to the next row
  repeat {
    # move along to the right until we find the first affected point on this row
    while (ic_run(x, y) == 0) {
      x <- x + 1
    }
    
    cat("\r", x, ", ", y, " (", x * 10000 + y - 99, ")\n", sep = "")
    # check to see if a 100x100 ship can fit in the tractor beam by seeing what
    # the output is for the point that is 99 to the right and 99 up. if we get a
    # 1 then the ship will fit
    if (ic_run(x+99, y-99) == 1) {
      return (x * 10000 + y - 99)
    }
    y <- y + 1
  }
}

part_two(ic)
