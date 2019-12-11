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

count_asteroids <- function(asteroids, progress = FALSE) {
  # add a column called count to the asteroids dataframe by iterating over each
  # of the values from the "a" column
  mutate(asteroids, count = map_dbl(a, function(A) {
    # filter the asteroids dataframe to just get the row where a == A
    A_df <- filter(asteroids, a == A)
    # find the coordinates of this asteroid
    Ax <- A_df$x
    Ay <- A_df$y
    
    # find all of the other asteroids, other than A
    ast <- asteroids %>% 
      filter(a != A) %>%
      # set the coordinates to be relative to A i.e. A = (0,0)
      mutate(x = x - Ax,
             y = y - Ay,
             # calculate the distance to A
             d = x^2 + y^2) %>%
      # sort by distance, so the closest is ordered first
      arrange(d)
    
    # build a list of all of the asteroids not visible from A
    not_visible <- numeric(0)
    # and a counter of the number of asteroids that are visible
    ctr <- 0
    
    # iterate over all of the asteroids in ast
    for(i in 1:nrow(ast)) {
      # if this asteroid is in the list not_visible, then skip over it
      if (!ast[i,]$a %in% not_visible) {
        # increment our counter
        ctr <- ctr + 1
        
        # get the distance from A to this asteroid (B) and the coordinates
        Bd <- ast[i,]$d
        Bx <- ast[i,]$x
        By <- ast[i,]$y
        
        # find the asteroids hidden by B: first check the special cases
        if (Bx == 0) {
          # B is directly above or below A
          nr <- filter(ast, x == 0)
        } else if (By == 0) {
          # B is directly left of right of A
          nr <- filter(ast, y == 0)
        } else {
          # find the slope from A->B, then find any points on that slope
          m <- By/Bx
          nr <- filter(ast, y == m*x)
        }
        
        # further filter our list:
        nr <- filter(nr,
                     # We need to find only those asteroids that are further
                     # from A than B is
                     d > Bd,
                     # We also need to ensure that we are filtering those that
                     # are in the same direction as A->B (i.e. not C->A->B, but
                     # A->B->C)
                     sign(x) == sign(Bx),
                     sign(y) == sign(By),
                     # We can skip any that are already in the not_visible list
                     !a %in% not_visible) %>%
          # pull just the id's of the asteroids
          pull(a)
        
        # add these to the list o not_visible
        not_visible <- c(not_visible, nr)
      }
    }
    # progress counter
    if (progress) cat(A, "/", max(asteroids$a),"\n")
    # return the number of visible asteroids
    return(ctr)
  }))
}

# examples

"
.#..#
.....
#####
....#
...##
" %>%
  str_trim() %>%
  read_lines() %>%
  get_asteroids() %>%
  count_asteroids()

"
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
" %>%
  str_trim() %>%
  read_lines() %>%
  get_asteroids() %>%
  count_asteroids() %>%
  arrange(desc(count)) %>%
  pull(count) %>%
  max() == 33

# --- Part One ---

part_one <- dt %>%
  get_asteroids() %>%
  count_asteroids(progress = TRUE) %>%
  arrange(desc(count))

part_one %>%
  pull(count) %>%
  max()

part_one %>% head(1) %>% select(x, y)
