library(tidyverse)

dt <- read_file("day07.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# We need to find all of the permutations of 0,1,2,3,4. There isn't a built-in
# function in R to do this, so I've borrowed the following function. source:
# https://www.r-bloggers.com/learning-r-permutations-and-combinations-with-base-r/
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}

# if a vector is given the class "op_list", then we have some custom index 
# functions to give us 0 based indexing - this massively simplifies the logic
# in the intcode computer
"[.op_list" <- function(x, i) {
  x[[i + 1]]
}

"[<-.op_list" <- function(x, i=NULL, value) {
  x[[i + 1]] <- value
  x
}

"print.op_list" <- function(x, p = 0, n = 4) {
  tmp <- unclass(x)
  cat ("p: ", p, " n: ", n, "\n")
  print(tmp[(p+1):(p+n)])
}

# the intcode computers now need to remember their state - so let's create a 
# class "intcode" that will hold the information we require to interupt and
# restart the computer
intcode_create <- function(data) {
  class(data) <- "op_list"
  res <- list(
    op_list = data,
    program_counter = 0,
    state = "not run",
    output = NA
  )
  class(res) <- "intcode"
  
  return(res)
}

# this is the function that runs the intcode computer. it will return the
# new state of the intcode computer
intcode_run <- function(intcode, inputs = numeric(0)) {
  op_list <- intcode$op_list
  p <- intcode$program_counter
  
  intcode$state <- "running"
  
  input_ctr <- 1
  
  tryCatch({
    while (op_list[p] != "99") {
      args <- list(
        ifelse(floor((op_list[p] %% 1000) / 100) == 1,
               op_list[p + 1],
               op_list[op_list[p + 1]]),
        ifelse(floor((op_list[p] %% 10000) / 1000) == 1 | op_list[p]%%100 %in% c(3, 4),
               op_list[p + 2],
               op_list[op_list[p + 2]])
      )
      
      instr <- op_list[p] %% 100
      
      if(instr == 1) {
        op_list[op_list[p + 3]] <- args[[1]] + args[[2]]
        p <- p + 4
      } else if(instr == 2) {
        op_list[op_list[p + 3]] <- args[[1]] * args[[2]]
        p <- p + 4
      } else if(instr == 3) {
        op_list[op_list[p + 1]] <- inputs[[input_ctr]]
        input_ctr <- input_ctr + 1
        p <- p + 2
      } else if(instr == 4) {
        intcode$op_list <- op_list
        intcode$program_counter <- p + 2
        intcode$output <- op_list[op_list[p + 1]]
        intcode$state <- "interupted"
        
        return(intcode)
      } else if(instr == 5) {
        if (args[[1]] != 0) {
          p <- args[[2]]
        } else {
          p <- p + 3
        }
      } else if(instr == 6) {
        if (args[[1]] == 0) {
          p <- args[[2]]
        } else {
          p <- p + 3
        }
      } else if(instr == 7) {
        op_list[op_list[p + 3]] <- ifelse(args[[1]] < args[[2]], 1, 0)
        p <- p + 4
      } else if(instr == 8) {
        op_list[op_list[p + 3]] <- ifelse(args[[1]] == args[[2]], 1, 0)
        p <- p + 4
      } else {
        stop("unkown op code")
      }
    }
    
    intcode$op_list <- op_list
    intcode$program_counter <- p
    intcode$state <- "complete"
    
    return (intcode)
  }, error = function(e) {
    cat("p: ", p, "\n", "op", op_list[p], "\n")
    stop(e)
  })
}

# --- Part One ---

part_one <- function(data, input) {
  intcode_computers <- list()
  for (i in seq_along(input)) {
    intcode_computers[[i]] <- intcode_create(data)
  }
  
  insig <- 0
  for(i in seq_along(input)) {
    intcode_computers[[i]] <- intcode_run(intcode_computers[[i]],
                                          c(input[[i]], insig))
    insig <- intcode_computers[[i]]$output
  }
  
  return (insig)
}

# examples

all (
  part_one(c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0),
           c(4,3,2,1,0)) == 43210,
  
  part_one(c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,
             23,1,24,23,23,4,23,99,0,0),
           c(0,1,2,3,4)) == 54321,
  
  part_one(c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0),
           c(1,0,4,3,2)) == 65210
)

# now, iterate through all of the permutations of 0:4 possible, and run the
# solve_fn function for that input. we will create a table that we can then use
# to find the maximum output
part_one_answer <- perm(0:4) %>%
  apply(1, function(input) part_one(dt, input)) %>%
  max()

# --- Part Two ---

part_two <- function(data, input) {
  intcode_computers <- list()
  for (i in seq_along(input)) {
    intcode_computers[[i]] <- intcode_create(data)
  }
  
  insig <- 0
  
  # loop 1
  for (i in seq_along(input)) {
    intcode_computers[[i]] <- intcode_run(intcode_computers[[i]],
                                          c(input[[i]], insig))
    insig <- intcode_computers[[i]]$output
  }
  # loop 2
  i <- 1
  while(intcode_computers[[i]]$state != "complete") {
    intcode_computers[[i]] <- intcode_run(intcode_computers[[i]], insig)
    insig <- intcode_computers[[i]]$output
    
    i <- (i %% length(intcode_computers)) + 1
  }
  
  return (intcode_computers[[length(intcode_computers)]]$output)
}

# examples

all (
  part_two(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
             27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5),
           c(9,8,7,6,5)) == 139629729,
  
  part_two(c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10),
           c(9,7,8,5,6)) == 18216
)

# run

part_two_answer <- perm(5:9) %>%
  apply(1, function(input) part_two(dt, input)) %>%
  max()


# show output

cat("Part One: ", part_one_answer, "\n",
    "Part Two: ", part_two_answer, "\n",
    sep = "")
