# --- Day 11: Space Police ---

library(tidyverse)

options(digits = 22)

dt <- read_file("day11.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# if a vector is given the class "op_list", then we have some custom index 
# functions to give us 0 based indexing - this massively simplifies the logic
# in the intcode computer
"[.op_list" <- function(x, i, r = 0) {
  k <- i + r + 1
  return (x[[k]])
}

"[<-.op_list" <- function(x, i, r = 0, value) {
  k <- i + r + 1
  x[[k]] <- value
  x
}

"print.op_list" <- function(x, p = 0, n = 4) {
  tmp <- unclass(x)
  cat ("p: ", p, " n: ", n, "\n")
  
  instructions <- c(
    "add",
    "multiply",
    "input",
    "output",
    "jump if true",
    "jump if false",
    "less than",
    "equals",
    "set relative pointer"
  )
  
  instr <- tmp[[p+1]] %% 100
  if (instr == 99) instr <- "HALT"
  else instr <- instructions[[instr]]
  cat ("instruction:", instr, "\n")
  
  for (i in tmp[(p+1):(p+n)]) {
    cat(i, ", ", sep = "")
  }
  cat("\n")
}

# the intcode computers now need to remember their state - so let's create a 
# class "intcode" that will hold the information we require to interupt and
# restart the computer

intcode_create <- function(data) {
  data <- c(data, numeric(100000))
  class(data) <- "op_list"
  res <- list(
    op_list = data,
    program_counter = 0,
    state = "not run",
    output = numeric(0),
    input = list(),
    cycles = 0,
    relmode_ptr = 0
  )
  class(res) <- "intcode"
  
  return(res)
}

intcode_add_input <- function(intcode, ...) {
  input <- list(...)
  for (i in input) {
    intcode$input <- append(intcode$input, i)
  }
  return(intcode)
}

"print.intcode" <- function(intcode) {
  cat("State: ", intcode$state, " (cycles: ", intcode$cycles, ")\n", sep = "")
  cat("Program Counter: ", intcode$program_counter,
      " (Rel Mode: ", intcode$relmode_ptr, ")\n", sep = "")
  cat("op_list:\n")
  print(intcode$op_list, p = intcode$program_counter)
  cat("Output: \n")
  print(intcode$output)
  cat("Input:\n")
  print(intcode$input)
}

intcode_run <- function(intcode,
                        completion = TRUE,
                        n = Inf) {
  
  while (intcode$state != "complete" & intcode$cycles < n) {
    op_list <- intcode$op_list
    p <- intcode$program_counter
    r <- intcode$relmode_ptr
    intcode$state <- "running"
    
    if (op_list[p] == 99) {
      intcode$state <- "complete"
      break
    }
    
    instr <- op_list[p] %% 100
    
    mode_fns <- list(
      "i" = function(pc) op_list[pc],
      "p" = function(pc) op_list[op_list[pc]],
      "r" = function(pc) op_list[op_list[pc], r]
    )
    
    mode_fn_select <- function(pos) {
      v <- floor((op_list[p] %% (pos*10)) / pos)
      case_when(v == 0 ~ "p",
                v == 1 ~ "i",
                v == 2 ~ "r")
    }
    
    mode_a <- mode_fns[[mode_fn_select(  100)]]
    mode_b <- mode_fns[[mode_fn_select( 1000)]]
    # lets not use get the function for c, we need to assign
    mode_c <- mode_fn_select(10000)
    
    save_to <- function(o) {
      if (mode_c == "i") return (p+o)
      i <- op_list[p+o]
      if (mode_c == "r") {
        i <- i + r
      }
      return (i)
    }
    
    new_val <- as.numeric(NA)
    if (instr == 1) {
      op_list[save_to(3)] <- mode_a(p+1) + mode_b(p+2)
      p <- p + 4
    } else if (instr == 2) {
      op_list[save_to(3)] <- mode_a(p+1) * mode_b(p+2)
      p <- p + 4
    } else if(instr == 3) {
      mode_c <- mode_fn_select(100)
      op_list[save_to(1)] <- intcode$input[[1]]
      intcode$input <- intcode$input[-1]
      p <- p + 2
    } else if(instr == 4) {
      intcode$output <- c(intcode$output, mode_a(p+1))
      intcode$state <- "interupted"
      p <- p + 2
    } else if(instr == 5) {
      if (mode_a(p+1) != 0) {
        p <- mode_b(p+2)
      } else {
        p <- p + 3
      }
    } else if(instr == 6) {
      if (mode_a(p+1) == 0) {
        p <- mode_b(p+2)
      } else {
        p <- p + 3
      }
    } else if(instr == 7) {
      op_list[save_to(3)] <- as.numeric(mode_a(p+1) <  mode_b(p+2))
      p <- p + 4    
    } else if(instr == 8) {
      op_list[save_to(3)] <- as.numeric(mode_a(p+1) == mode_b(p+2))
      p <- p + 4    
    } else if (instr == 9) {
      intcode$relmode_ptr <- r + mode_a(p+1)
      p <- p + 2
    } else {
      print(intcode)
      stop("unkown op code")
    }
    
    intcode$op_list <- op_list
    intcode$program_counter <- p
    intcode$cycles <- intcode$cycles + 1
    
    if (!completion && intcode$state == "interupted") break
  }
  
  intcode
}

intcode_output <- function(intcode) intcode$output


# examples

print_board <- function(board, p, d) {
  b <- apply(board, c(1,2), function(x) ifelse(x == 1, "#", "."))
  
  if (all(d[,1] == c( 0, -1))) {
    b[p[2,1], p[1,1]] <- "^"
  } else if (all(d[,1] == c(-1,  0))) {
    b[p[2,1], p[1,1]] <- "<"
  } else if(all(d[,1] == c( 0,  1))) {
    b[p[2,1], p[1,1]] <- "V"
  } else if (all(d[,1] == c( 1,  0))) {
    b[p[2,1], p[1,1]] <- ">"
  }
  
  b %>%
    apply(1, function(x) paste0(x, collapse = "")) %>%
    paste0(collapse="\n") %>% cat()
}

painter <- function(board_size, init = 0) {
  ic <- intcode_create(dt)
  
  board <- matrix(0, board_size, board_size)
  
  # initialize position
  p <- matrix(board_size/2, 2)
  
  board[p[2,1], p[1,1]] <- init
  
  # initilize direction pointing (start up)
  #              init   | left90 | right90
  # move up:      0, -1 | -1,  0 |  1,  0
  # move left:   -1,  0 |  0,  1 |  0, -1
  # move down:    0,  1 |  1,  0 | -1,  0
  # move right:   1,  0 |  0, -1 |  0,  1
  d <- matrix(c(0, -1), nrow = 2)
  
  # coordinates in a matrix are row, column, so reverse our p
  
  i <- 1
  
  painted <- matrix(0, board_size, board_size)
  while (ic$state != "complete") {
  
    ic <- ic %>%
      intcode_add_input(board[p[2,1], p[1,1]]) %>%
      intcode_run(FALSE) %>%
      intcode_run(FALSE)
    
    if (ic$state == "complete") break
    
    ic$output
    
    board[p[2,1], p[1,1]] <- ic$output[[2*i-1]]
    if (ic$output[[2*i-1]] == 1) {
      painted[p[2,1], p[1,1]] <- 1
    }
    
    if (ic$output[[2*i]] == 1) {
      # right 90
      d <- matrix(c(0, 1, -1, 0), nrow = 2) %*% d
    } else {
      # left 90
      d <- matrix(c(0, -1, 1, 0), nrow = 2) %*% d
    }
    
    p <- p + d
    i <- i + 1
    
    if (i %% 100 == 0) {
      cat("Iter:", i, "Painted:", sum(painted), "\n")
    }
  }
  
  
  return(list(painted = sum(painted),
              iter = i,
              board = board,
              ic = ic,
              p = p,
              d = d))
}


part_one <- painter(10000, 0)
part_one$painted

part_two <- painter(90, 1)

print_board(part_two$board, part_two$p, part_two$d)
