# --- Day 20: Donut Maze ---

library(tidyverse)
library(datastructures)
library(igraph)

dt <- read_lines("day20.txt")

maze_size <- c(str_length(dt[[1]]), length(dt))
# figure out the outer size of the maze: there is a 2 character padding all
# around for the letters
maze_outer <- maze_size - 4

# find the maze middle size, count the number of # and . on the line

# first, find out the mid point
maze_middle <- ceiling(maze_size / 2)

# then, work out how wide the left and right bit of the maze is, then the top
# and bottom bit of the maze is (at the midpoint)
maze_middle <- c(
  map_chr(dt, str_sub, maze_middle[[1]], maze_middle[[1]]) %>%
    paste(collapse = "") %>% {
      c(str_sub(., 1, maze_middle[[2]]),
        str_sub(., maze_middle[[2]]))
    },
  
  dt[[maze_middle[[2]]]] %>% {
    c(str_sub(., 1, maze_middle[[1]]),
      str_sub(., maze_middle[[1]]))
  }
) %>%
  str_extract_all("\\.|#") %>%
  map_dbl(length)

# now, update maze_middle to be the square inside 
maze_middle <- (maze_middle + c(3, +2-maze_size[[1]],
                                3, +2-maze_size[[2]]))*c(1,-1,1,-1)

# now, build a list of the named points
get_portals <- function(maze) {
  portals <- list()

  # row loop
  for (i in maze_middle[[3]]:maze_middle[[4]]) {
    # left out
    if (maze[i, 1] != " ") {
      v <- paste0(paste(maze[i, 1:2], collapse = ""), "o")
      portals[[v]] <- c(portals[[v]], 3, i)
      
      maze[i, 2] <- v
    }
    
    # right out
    if (maze[i, maze_size[[1]]] != " ") {
      v <- paste0(paste(maze[i, maze_size[[1]]-c(1,0)], collapse = ""), "o")
      portals[[v]] <- c(portals[[v]], maze_size[[1]]-2, i)
      
      maze[i, maze_size[[1]]-1] <- v
    }
    
    # left middle
    if (maze[i, maze_middle[[1]]] != " ") {
      v <- paste0(paste(maze[i, maze_middle[[1]] + c(0,1)], collapse = ""), "i")
      portals[[v]] <- c(portals[[v]], maze_middle[[1]]-1, i)
      
      maze[i, maze_middle[[1]]] <- v
    }
    
    # right middle
    if (maze[i, maze_middle[[2]]] != " ") {
      v <- paste0(paste(maze[i, maze_middle[[2]] - c(1,0)], collapse = ""), "i")
      portals[[v]] <- c(portals[[v]], maze_middle[[2]]+1, i)
      
      maze[i, maze_middle[[2]]] <- v
    }
  }
  
  # column loop
  for (i in maze_middle[[1]]:maze_middle[[2]]) {
    # top out
    if (maze[1, i] != " ") {
      v <- paste0(paste(maze[1:2, i], collapse = ""), "o")
      portals[[v]] <- c(portals[[v]], i, 3)
      
      maze[2, i] <- v
    }
    
    # bottom out
    if (maze[maze_size[[2]], i] != " ") {
      v <- paste0(paste(maze[maze_size[[2]]-c(1,0), i], collapse = ""), "o")
      portals[[v]] <- c(portals[[v]], i, maze_size[[2]]-2)
      
      maze[maze_size[[2]]-1, i] <- v
    }
    
    # middle top
    if (maze[maze_middle[[3]], i] != " ") {
      v <- paste0(paste(maze[maze_middle[[3]] + c(0,1), i], collapse = ""), "i")
      portals[[v]] <- c(portals[[v]], i, maze_middle[[3]]-1)
      
      maze[maze_middle[[3]], i] <- v
    }
    
    # middle bottom
    if (maze[maze_middle[[4]], i] != " ") {
      v <- paste0(paste(maze[maze_middle[[4]] - c(1,0), i], collapse = ""), "i")
      portals[[v]] <- c(portals[[v]], i, maze_middle[[4]]+1)
      
      maze[maze_middle[[4]], i] <- v
    }
  }
  
  list(maze = maze, portals = portals)
}

# create a graph data frame from the maze and portals
build_graph <- function(maze, portals) {
  # quickly sort the portals
  portals <- portals[sort(names(portals))]
  
  q <- queue()
  directions = list(
    L = c(-1,  0),
    R = c( 1,  0),
    U = c( 0, -1),
    D = c( 0,  1)
  )
  
  nodes <- list()
  intersections <- 0
  while (length(portals) > 0) {
    start <- names(portals)[[1]]
    
    insert(q, list(list(x = portals[[start]][[1]],
                        y = portals[[start]][[2]],
                        f = start,
                        d = 0)))
    
    portals[[start]] <- NULL
    
    # the initial point wont work in the loop, so let's sort it
    nodes[[start]] <- list(from = "*", to = start, weight = 0)
    
    while (size(q) > 0) {
      n <- pop(q)[[1]]
      from <- n$f
      
      v <- maze[n$y, n$x]
      
      # mark as visited
      maze[n$y, n$x] <- "#"
      
      if (v != ".") {
        nodes[[v]] <- list(from = from, to = v, weight = n$d-1)
        portals[[v]] <- NULL
        from <- n$v
      }
      
      moves <- map(directions, ~maze[n$y + .x[[2]], n$x + .x[[1]]]) %>%
        keep(~.x == "." || !is.null(portals[[.x]]))
      
      if (length(moves) == 0) {
        # dead end
      } else if (length(moves) == 1) {
        # one single move
        m <- names(moves)[[1]]
        d <- directions[[m]]
        
        nx <- n$x + d[[1]]
        ny <- n$y + d[[2]]
        
        insert(q, list(list(x = nx,
                            y = ny,
                            f = from,
                            d = n$d + 1)))
      } else {
        # multiple moves, build an intersection
        intersections <- intersections + 1
        v <- paste0("ix", str_pad(intersections, 3, pad = "0"))
        nodes <- c(nodes, list(list(from = from, to = v, weight = n$d)))
        from <- v
        
        for (m in names(moves)) {
          d <- directions[[m]]
          
          nx <- n$x + d[[1]]
          ny <- n$y + d[[2]]
          
          insert(q, list(list(x = nx,
                              y = ny,
                              f = from,
                              d = 1)))
        }
      }
    }
  }
  # convert to a tibble
  bind_rows(nodes)
}

simplify_graph <- function(g) {
  # remove any intersection that results in a dead-end. Recursively call this
  # until we have no intersections with degree 1 (i.e. only one way in and out)
  repeat {
    dv <- V(g)[degree(g) == 1]$name
    dv <- dv[str_length(dv) == 5]
    if (length(dv) == 0) break
    g <- delete_vertices(g, dv)
  }
  
  cmps <- components(g)$membership %>%
    map_dfr(~tibble(c = .x), .id = "v") %>%
    group_nest(c, .key = "v") %>%
    mutate_at(vars(v), map, pull, quo(v)) %>%
    pull(v) %>%
    map(~.x[str_length(.x) == 3])
  
  res <- list()
  for (cmp in cmps) {
    for (i in 1:(length(cmp)-1)) {
      for (j in (i+1):length(cmp)) {
        d <- distances(g, cmp[[i]], cmp[[j]])
        res[[length(res)+1]] <- list(from   = cmp[[i]],
                                     to     = cmp[[j]],
                                     weight = d[[1]])
      }
    }
  }
  bind_rows(res) %>%
    graph_from_data_frame(FALSE)
}

gp <- get_portals(str_split(dt, "", simplify = TRUE))

g <- build_graph(gp$maze, gp$portals) %>%
  filter(from != "*") %>%
  graph_from_data_frame(FALSE) %>%
  simplify_graph()

part_one <- function(g) {
  # find all of the portals, and directly connect them together with an edge
  # weight of 1
  portal_edges <- V(g)$name %>%
    # get rid of the start and end portals
    subset(!. %in% c("AAo", "ZZo")) %>%
    # arrange them alphabetically, so each pair of portals are adjacent
    sort()
  
  # add an edge between portal pairs with a weight of 1: it takes one step to
  # move throught the portals
  g <- add_edges(g, portal_edges, weight = 1)
  
  # find the distance from the start portal to the end portal
  distances(g, "AAo", "ZZo")[[1]]
}

part_two <- function(g, depth = 200) {
  # create a copy of the graph: we are going to build ng up layer by layer,
  # connecting the inner portals to the outer portals
  ng <- g
  
  # initial level
  # rename vertices to include what depth they take us to
  for(v in V(ng)$name) {
    a <- str_sub(v, 1, 2)
    b <- ifelse(str_sub(v, -1) == "i", 2, 0)
    vertex_attr(ng, "name", v) <- paste0(a, b)
  }
  
  # for each subsequent depth, use the following copy of the original graph
  # remove AA/ZZ, these only exist on level 0
  dg <- delete.vertices(g, c("AAo", "ZZo"))
  # increase each weight by 1 to account for the step through the portal. We do
  # this because we are going to directly connect the portals from now on, so
  # there won't be a "step" between portals
  edge_attr(dg, "weight_x") <- edge_attr(dg, "weight") + 1
  # delete the weight edge attribute: when we union two graphs together it
  # needs unique edge attribute names, otherwise it can't properly merge
  dg <- delete_edge_attr(dg, "weight")
  
  # now loop through the rest of the levels we want to create
  for (i in 2:depth) {
    # create a copy of the dg graph for this level
    dgx <- dg
    # rename in vertices to depth+1, out vertices to depth-1
    for(v in V(dgx)$name) {
      a <- str_sub(v, 1, 2)
      b <- ifelse(str_sub(v, -1) == "i", i+1, i-1)
      vertex_attr(dgx, "name", v) <- paste0(a, b)
    }
    
    # combine the graphs ng and dgx
    ng <- graph.union(ng, dgx)
    
    # recalculate the edge weights: the items from ng will have values for the
    # "weight" attribute, but NA's for "weight_x", and vice-versa for dgx. We
    # need to replace NA's with 0's.
    vw1 <- edge_attr(ng, "weight"  ) %>% replace_na(0)
    vw2 <- edge_attr(ng, "weight_x") %>% replace_na(0)
    
    # update the edge weights, and delete the extra weight attibute
    edge_attr(ng, "weight") <- vw1+vw2
    ng <- delete_edge_attr(ng, "weight_x")
  }
  
  # use Dijkstra's to calculate the distance from the start portal to the end
  # we need to add one extra step through to ZZ
  distances(ng, "AA0", "ZZ0")[[1]] + 1
}

part_one(g)
# Part 2 should be solved with an infinite graph, but R is just too slow. A good
# example of this working in Python is provided by the following post on Reddit:
# https://www.reddit.com/r/adventofcode/comments/ed5ei2/2019_day_20_solutions/fbm3gfx/
# Instead, I solve this by copying the graph n times, and then connecting each
# layer of the graph by the portals. I rename the portals at each level so they
# connect, then use igraph to calculate the distance (using Dijkstra's)
part_two(g, depth = 51)
