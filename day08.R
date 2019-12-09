# --- Day 8: Space Image Format ---

library(tidyverse)

dt <- read_lines("day08.txt")

# we are told the images are 25x6
image_w <- 25
image_h <-  6
# this means that we have 150 pixels per layer
image_p <- image_w*image_h
# we now work out how many layers the image will have
image_l <- str_length(dt) / image_p

# --- Part One ---

# split the input up into individual characters and convert them to numbers
dt.split <- str_split(dt, "", simplify = TRUE)[1,] %>%
  as.numeric()

# we need to take the 1:150, 151:300, 301:350... items from our input and
# convert these to a matrix that is 25x6. make sure to set the byrow = TRUE
# argument otherwise your image will be weirdly rotated
layers <- seq(1, image_l*image_p, image_p) %>%
  map(~dt.split[.x:(.x+image_p-1)] %>%
        matrix(image_h, image_w, TRUE))

# iterate through each layer. use the table function to count how many of each
# value each in that layer, convert it to a list, then we can convert the result
# to a tibble. these will then be unioned into one single table
map_dfr(layers, compose(as_tibble, as.list, table)) %>%
  # sort the results so the layer with the fewest 0's is first
  arrange(`0`) %>%
  # multiply the 1's and 2's
  mutate(value = `1`*`2`) %>%
  # take the first row only i.e. the one with the fewest 0's
  head(1) %>%
  # get the result of the value we just calculated
  pull(value)

# --- Part Two ---

# in the previous part we created a list where each layer was a matrix. let's
# now collapse these matrices into a single matrix
part_two_answer <- matrix(0, image_h, image_w)
# this means a bunch of nested loops, one for the x direction
for (x in 1:image_w) {
  # one for the y direction
  for (y in 1:image_h) {
    # we now iterate through the layers
    for (i in 1:image_l) {
      # get the value for this layer
      v <- layers[[i]][y,x]
      # if it's a 2, continue onto the next layer... otherwise
      if (v != 2) {
        # set this pixel's value to a space if it's a white pixel, or an "@" if
        # its a black pixel
        part_two_answer[y,x] <- ifelse(v == 0, " ", "@")
        # break out of the layers loop, back to the y direction loop (2nd loop)
        break()
      }
    }
  }
}

# now we can collapse the matrix into a single string and use the "cat" function
# to dump the result to the screen
part_two_answer %>%
  # for each of the rows, collapse the columns into a single string with no
  # characters between the strings
  apply(1, paste0, collapse = "") %>%
  # now collapse the rows into a single string, join the rows with a newline
  # character (\n)
  paste0(collapse = "\n") %>%
  cat()
