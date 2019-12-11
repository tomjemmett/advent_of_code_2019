# take all points
# remove the point of interest
# calculate distance from point of interest to all other points
# sort based on this distance
# in order, take a point and compare it to every point below it:
# if the two points and the point of interest are on the same line, then
# remove the later point

library(tidyverse)

dt <- read_lines("day10.txt")

get_asteroids <- function(data) {
  data %>%
  str_split("") %>%
  imap_dfr(function(x, r) {
    imap_dfr(x, function(v, c) {
      tibble(x = c-1, y = r-1, v = v)
    })
  }) %>%
  filter(v == "#") %>%
  select(-v) %>%
  mutate(a = row_number())
}

# --- Part One ---

dt.asteroids <- dt %>%
  get_asteroids()

part_one <- dt.asteroids %>%
  # iterate over ever asteroid, placing the station at that station
  mutate(visible = map_dbl(a, function(A) {
    # find the asteroid in quesiton
    A_df <- filter(dt.asteroids, a == A)
    
    dt.asteroids %>%
      # get rid of the asteroid that the station is at
      filter(a != A) %>%
      # set the location of every other asteroid to be relative to the station
      mutate(x = x-A_df$x,
             y = y-A_df$y,
             # calculate the distance from the station
             d = x^2+y^2,
             # calculate the angle to the asteroid from the station, clockwise, with
             # 0 = straight up, pi/2 = right, pi = down, 1.5 pi = left
             theta = (atan2(y, x) + 2.5*pi) %% (2*pi)) %>%
      # group by the angles
      group_by(theta) %>%
      # add a rank based on how far we are from the station
      mutate(r = rank(d)) %>%
      # find only the rank 1 rows, i.e. the rows we reach first
      filter(r == 1) %>%
      # return the number of rowws
      nrow()
  })) %>%
  # arrange the data by the number of rows
  arrange(desc(visible))

# find the answer to part one
part_one %>%
  pull(visible) %>%
  max()

part_one_solution <- part_one %>% head(1)

# --- Part Two ---

# we start from pointing directly up, rotate clockwise, and knock off one
# asteroid at a time, before rotating clockwise.
dt.asteroids %>%
  # get rid of the asteroid that the station is at
  filter(a != part_one_solution$a) %>%
  # set the location of every other asteroid to be relative to the station
  mutate(x = x-part_one_solution$x,
         y = y-part_one_solution$y,
         # calculate the distance from the station
         d = x^2+y^2,
         # calculate the angle to the asteroid from the station, clockwise, with
         # 0 = straight up, pi/2 = right, pi = down, 1.5 pi = left
         theta = (atan2(y, x) + 2.5*pi) %% (2*pi)) %>%
  # group by the angles
  group_by(theta) %>%
  # add a rank based on how far we are from the station
  mutate(r = rank(d)) %>%
  # get rid of the grouping
  ungroup() %>%
  # arrange first based on the rank, then on the angle
  arrange(r, theta) %>%
  # add a row number for when we hit the asteroid
  mutate(k = row_number()) %>%
  # find the 200th
  filter(k == 200) %>%
  # calculate the answer
  mutate(answer = (x+part_one_solution$x)*100+y+part_one_solution$y) %>%
  # show the answer!
  pull(answer)
