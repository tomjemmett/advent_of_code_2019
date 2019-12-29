# --- Day 24: Planet of Discord ---

library(tidyverse)
library(hashmap)

# A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
# An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.

dt <- read_lines("day24.txt")

cat(paste(dt, collapse = "\n"))

# convert data to a matrix, give a 1 pixel buffer

dtm <- rbind(rep(0, 7),
             cbind(rep(0, 5),
                   ifelse(str_split(dt, "", simplify = TRUE) == "#", 1, 0),
                   rep(0,5)),
             rep(0, 7))

class(dtm) <- "d22"

print.d22 <- function(dtm) {
  ch <- c(".", "#")
  for (r in 2:6) {
    for (c in 2:6) {
      cat(ch[[dtm[r, c]+1]])
    }
    cat("\n")
  }
}

# --- Part One ---

part_one <- function(dtm) {
  # iteration function for updating the matrix each time
  iter <- function(dtm) {
    # take a copy of the matrix for the updated values
    dtm.ret <- dtm
    for (r in 2:6) {
      for (c in 2:6) {
        a <- dtm[r, c]
        # sum the adjacent cells
        v <- sum(dtm[r-1,c],dtm[r,c-1],
                 dtm[r+1,c],dtm[r,c+1])
        
        if (a == 1) {
          # stays alive iff there is only 1 adjacent neighbour
          a <- ifelse(v == 1, 1, 0)
        } else {
          # becomes alive iff there is one or two adjacent neighbours
          a <- ifelse(v == 1 | v == 2, 1, 0)
        }
        
        dtm.ret[r, c] <- a
      }
    }
    dtm.ret
  }
  
  # function for calculing the biodiverity rating
  biodiversity <- function(dtm) {
    # get rid of the padding from dtm, transpose, the convert to a vector
    v <- dtm[2:6, 2:6] %>% t() %>% as.vector()
    # get the powers of 2 starting from 2^0 (1)
    m <- 2^(seq_along(v)-1)
    # sum the product of the two vectors: because v is either 0 or 1 this has
    # the effect of either including the power of 2 at that position, or not
    sum(v*m)
  }
  # let's use a hashmap to store the biodiversity scores
  hm <- hashmap(numeric(0), logical(0))
  
  # keep track of time
  ctr <- 0
  
  repeat {
    # calculate the biodiversity
    b <- biodiversity(dtm)
    
    # check to see if we have seen this before
    if (hm$has_key(b)) {
      cat("\n\nFound after", ctr, "minutes\n")
      return (b)
    }
    # add this biodiversity rating to our hashmap 
    hm[[b]] <- TRUE
    
    # iterate
    dtm <- iter(dtm)
    # increment the counter
    ctr <- ctr + 1
  }
}

part_one(dtm)

# --- Part Two ---

part_two <- function(dtm, m) {
  # create a template for each new level
  new_level <- matrix(0, 7, 7)
  class(new_level) <- "d22"
  
  # level 1 is outer most level, n is inner most level. mid point is initial
  # we always want to keep a buffer of levels to the left and right to simplify
  # the loop later.
  levels <- list( new_level, dtm, new_level )
  
  # function to handle iterating a cell from one state to the next
  iter <- function(levels, level, r, c) {
    a <- levels[[level]][r, c]
    v <- 0
    # what value is up
    if (r == 2) {
      v <- v + levels[[level-1]][3, 4]
    } else if (r == 5 && c == 4) {
      v <- v + sum(levels[[level+1]][6,])
    } else {
      v <- v + levels[[level]][r-1, c]
    }
    
    # what value is down
    if (r == 6) {
      v <- v + levels[[level-1]][5, 4]
    } else if (r == 3 && c == 4) {
      v <- v + sum(levels[[level+1]][2,])
    } else {
      v <- v + levels[[level]][r+1, c]
    }
    
    # what value is left
    if (c == 2) {
      v <- v + levels[[level-1]][4, 3]
    } else if (c == 5 && r == 4) {
      v <- v + sum(levels[[level+1]][,6])
    } else {
      v <- v + levels[[level]][r, c-1]
    }
    
    # what value is right
    if (c == 6) {
      v <- v + levels[[level-1]][4, 5]
    } else if (c == 3 && r == 4) {
      v <- v + sum(levels[[level+1]][,2])
    } else {
      v <- v + levels[[level]][r, c+1]
    }
    
    # calculate whether the cell should be alive of dead
    if (a == 1) {
      # stays alive iff there is only 1 adjacent neighbour
      a <- ifelse(v == 1, 1, 0)
    } else {
      # becomes alive iff there is one or two adjacent neighbours
      a <- ifelse(v == 1 | v == 2, 1, 0)
    }
    
    # return the value of whether this cell is alive or not
    a
  }
  
  # keep track of how many minutes have passed
  for (ctr in 0:(m-1)) {
    # because the scan is of a 5x5 patch, we only need to bother adding a level
    # every other minute, because we wont yet have reached the middle of the
    # lower level/outer of upper level
    if (!ctr %% 2) {
      levels <- c(list(new_level), levels, list(new_level))
    }
    
    # take a copy of levels
    new_levels <- levels
    
    # skip the inner most and outer most levels
    for (i in 2:(length(levels)-1)) {
      for (r in 2:6) {
        for (c in 2:6) {
          # skip centre
          if (r == 4 && c == 4) next()
          # update the current value using the iter function
          new_levels[[i]][r, c] <- iter(levels, i, r, c)
        }
      }
    }
    # set levels to the updated levels
    levels <- new_levels
    
    # count how many bugs are alive in each of the levels by reducing summing
    # each matrix and adding it to the previous value
    bugs_alive <- reduce(levels, sum, .init = 0)
    
    # show some progres
    cat("\rt=", str_pad(ctr, 3), ", v=", str_pad(bugs_alive, 4), sep="")
  }
  cat("\n")
  
  # return how many bugs are alive
  bugs_alive
}

part_two(dtm, 200)
