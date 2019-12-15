# --- Day 14: Space Stoichiometry ---

library(tidyverse)
library(igraph)
library(ggraph)

dt <- read_lines("day14.txt")

ex1 <- "
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
" %>% str_trim() %>% read_lines()

ex2 <- "
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
" %>% str_trim() %>% read_lines()

ex3 <- "
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
" %>% str_trim() %>% read_lines()

plot_graph <- function(g) {
  p <- g %>%
    ggraph() +
    geom_edge_link(aes(label = weight),
                   angle_calc = 'along',
                   label_dodge = unit(2.5, 'mm'),) +
    geom_node_label(aes(label = paste0(name, ", ", weight)))
  print(p)
}

create_graph <- function(input) {
  # take our input and split it up into a data frame
  df <- input %>%
    str_split(" => ") %>%
    imap_dfr(function(s, i) {
      tibble(x = str_split(s[[1]], ", ", simplify = TRUE)[1,],
             y = s[[2]],
             i = i)
    }) %>%
    separate(x, c("in_c", "in_n"), " ") %>%
    separate(y, c("out_c", "out_n"), " ") %>%
    mutate_at(vars("in_c", "out_c"), as.numeric)
  
  # now, create a directed graph (we are going to reverse the direction of the
  # graph though, so we search FUEL to ORE)
  g <- df %>%
    select(out_n, in_n) %>%
    graph_from_data_frame(directed = TRUE)
  
  # quickly verify that our vertices have been created in the same order as our
  # data frame
  # if so, assign to each vertex the value of how many we create
  if (all(V(g)$name != c(distinct(df, out_n, out_c, i)$out_n, "ORE"))) {
    stop("cannot assign node weights correctly")
  } else {
    V(g)$weight <- c(distinct(df, out_n, out_c, i)$out_c, 1)
  }
  
  # the amount of resources required = the weight of the edge
  E(g)$weight <- df$in_c
  
  # return the graph
  g
}

# --- Part One ---

part_one_solve <- function(g, fuel_required = 1) {
  # first, build the order that we need to traverse the vertices.
  # we need to hit each vertex at a point that we wont reach it later, e.g. we
  # create a topological sort
  # create a list for the visited vertices, and a stack that we pop/push the
  # vertices to
  visited <- list()
  # start off with the fuel node
  stack <- list("FUEL")
  
  # stacks are FIFO: first in, first out
  
  # while we have items in the stack
  while (length(stack) > 0) {
    # remove the first item in the stack
    vertex <- stack[[1]]
    # and remove this item from the stack (it may appear multiple times)
    stack <- stack[stack != vertex]
    
    # find the neighbours coming in
    n_in  <- neighbors(g, vertex, "in")$name
    # find the neighbours coming out
    n_out <- neighbors(g, vertex, "out")$name
    
    # append the vertices going out to the stack, we will visit these later
    stack <- append(stack, n_out)
    
    # filter the in vertices to be those that we haven't yet visited
    n_in <- n_in[!n_in %in% visited]
    # if all of the vertices pointing in have been visted, then add this vertex
    # to the visited list
    if (length(n_in) == 0) {
      visited <- append(visited, vertex)
    } else {
      # otherwise, we need to come back to this vertex later, put it to the end
      # of the stack
      stack <- append(stack, vertex)
    }
    
    stack
  }
  
  # remove the "ORE" vertex from the list of visited vertices, we don't need to
  # visit this again
  visited <- visited[visited != "ORE"]
  
  # build a vector for the vertices that counts how many of that vertex we need
  required <- numeric(length(V(g)))
  names(required) <- V(g)$name
  # initialize the fuel item only
  required[["FUEL"]] <- fuel_required
  
  # build a vector for the vertices that keeps track of how many we have created
  create <- numeric(length(V(g)))
  names(create) <- V(g)$name
  
  # iterate through the vertices in the topological sort order we found above
  for (vertex in visited) {
    # work out how many extra items we require to create, that is the required
    # amount for this vertex should be divisible by the amount that we create of
    # this vertex
    extra <- (-required[[vertex]] %% V(g)[[vertex]]$weight)
    # update the required and the create vectors
    required[[vertex]] <- required[[vertex]] + extra
    create[[vertex]] <- required[[vertex]] / V(g)[[vertex]]$weight
    
    # find the neighbours and the edges
    n <- neighbors(g, vertex)
    e <- incident_edges(g, vertex)[[1]]
    
    # work out how many of the neighbours we need to create
    r <- e$weight * create[[vertex]]
    
    # update the neighbors required amounts
    required[n$name] <- required[n$name] + r 
  }
  
  # now, calculate for the ore how many we need
  ore_n <- neighbors(g, "ORE", "in")
  ore_r <- required[ore_n$name] / ore_n$weight
  ore_w <- incident_edges(g, "ORE", "in")[[1]]$weight
  
  sum(ore_r * ore_w)
}
# examples
create_graph(ex1) %>% part_one_solve() == 31
create_graph(ex2) %>% part_one_solve() == 165
create_graph(ex3) %>% part_one_solve() == 13312

dt.g <- create_graph(dt)
part_one <- part_one_solve(dt.g)

part_one

# --- Part Two ---

# use a divide and conquer technique to find the solution:
# if we start with a worst case scenario of generating 1 fuel (the left), and a
# best case of creating as much fuel as the ore_limit (the right), we can then
# try solving the problem with the mid point of the left and right.

part_two  <- 0
ore_limit <- 1000000000000
right     <- ore_limit
left      <- 1
# if we ever set the left to be greater than the right, the right less than
# the left, or the left = right, then we have found the solution
while (left <= right) {
  mid <- round((left + right) / 2)
  
  required_ore <- part_one_solve(dt.g, mid)
  # if we haven't used as much ore as our limit, then set the left case to be 1
  # greater than the best case, and update our part_two value
  if (required_ore < ore_limit) {
    part_two <- max(part_two, mid)
    left <- mid + 1
  # in this case the solution found requires more ore than we have available, so
  # sest the right value to be one less than the mid value.
  } else if (required_ore > ore_limit) {
    right <- mid - 1
  # we have found a perfect solution!
  } else {
    part_two <- mid
  }
}

part_two
