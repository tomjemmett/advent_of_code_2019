# --- Day 6: Universal Orbit Map ---

library(tidyverse)
library(igraph)

# This is a problem that can be sovled neatly using graphs, so let's use the
# wonderful igraph package :-)

# read in the data, convert it to a dataframe where the text before the ")" is
# put into a column called "to", and the text after into a column called "from"
dt <- read_lines("day06.txt")
dt.df <- tibble(e = dt) %>%
  separate(e, c("to", "from"), "\\)")

# we can now create a graph from the data frame. for part 2 we are going to need
# the graph to be "undirected", so set directed = FALSE
g <- graph_from_data_frame(dt.df, directed = FALSE)

# --- Part One ---

# find the distance from every vertex in g [V(g)] to the vertext "COM", then sum
# the distances
distances(g, v = V(g), to = "COM") %>% sum()

# --- Part Two ---

# find the shortest path from the vertex "YOU" to the vertex "SAN". We need to
# know the length of the first found path (it will be the only one in this case)
# and subtract 3 from it - 1 for "YOU", 1 for "SAN", and 1 for the vertex we are
# currently orbitting
shortest_paths(g, "YOU", "SAN")$vpath[[1]] %>% length() - 3

