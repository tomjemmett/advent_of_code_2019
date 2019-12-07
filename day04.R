# --- Day 4: Secure Container ---
  
library(tidyverse)
numbers_all <- 165432:707912

# --- Part One ---

# Brute force approach: I'm sure there is a better way of achieving this, but
# this works! :-) it does take some time though, about 90s on my machine

# start off with all the numbers, set the names of the vector to be the values
# and then split the values into indivdual numbers. 
part_one <- numbers_all %>%
  set_names() %>%
  map(str_split, "", simplify = TRUE) %>%
  map(~as.numeric(.x[1,])) %>%
  # calculate the lengths of runs of numbers in the sequence
  map(rle) %>%
  # if any of the values decrease we get a negative value, so discard these
  discard(~any(diff(.x$values) < 0)) %>%
  # if any of the values are the same we get 0, so keep these
  keep(~any(.x$lengths >= 2))

length(part_one)

# --- Part Two ---

# we have already calculated the run lengths, now we need to only keep numbers
# where there is a run length of exactly 2
part_two <- part_one %>%
  keep(~any(.x$lengths == 2))

length(part_two)
