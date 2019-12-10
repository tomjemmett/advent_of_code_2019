# --- Day 9: Sensor Boost ---
  
library(tidyverse)

options(digits = 22)

dt <- read_file("day09.txt") %>%
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
    output = NA,
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
  cat("Output: ", intcode$output, "\n")
  cat("Input:\n")
  print(intcode$input)
}

intcode_run_cycle <- function(intcode, immediate_output = FALSE) {
  op_list <- intcode$op_list
  p <- intcode$program_counter
  r <- intcode$relmode_ptr
  intcode$state <- "running"
  
  if (op_list[p] == 99) {
    intcode$state <- "complete"
    return(intcode)
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
  
  new_val <- as.numeric(NA)
  if (instr == 1) {
    new_val <- mode_a(p+1) + mode_b(p+2)
    nvp <- p + 3
    p <- p + 4
  } else if (instr == 2) {
    new_val <- mode_a(p+1) * mode_b(p+2)
    nvp <- p + 3
    p <- p + 4
  } else if(instr == 3) {
    mode_c <- mode_fn_select(100)
    new_val <- intcode$input[[1]]
    intcode$input <- intcode$input[-1]
    nvp <- p + 1
    p <- p + 2
  } else if(instr == 4) {
    intcode$output <- mode_a(p+1)
    if (immediate_output) cat(">", intcode$output, "\n")
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
    new_val <- as.numeric(mode_a(p+1) <  mode_b(p+2))
    nvp <- p + 3
    p <- p + 4    
  } else if(instr == 8) {
    new_val <- as.numeric(mode_a(p+1) == mode_b(p+2))
    nvp <- p + 3
    p <- p + 4    
  } else if (instr == 9) {
    intcode$relmode_ptr <- r + mode_a(p+1)
    p <- p + 2
  } else {
    print(intcode)
    stop("unkown op code")
  }
  
  if (!is.na(new_val)) {
    if (mode_c == "r") {
      op_list[op_list[nvp], r] <- new_val
    } else if (mode_c == "p") {
      op_list[op_list[nvp]] <- new_val
    } else {
      op_list[nvp] <- new_val
    }
  }
  
  intcode$op_list <- op_list
  intcode$program_counter <- p
  intcode$cycles <- intcode$cycles + 1
  
  return(intcode)
}

intcode_run <- function(intcode,
                        completion = TRUE,
                        immediate_output = FALSE ,
                        n = Inf) {
  cycle <- 0
  while (intcode$state != "complete" & cycle < n) {
    intcode <- intcode_run_cycle(intcode, immediate_output)
    if (!completion && intcode$state == "interupted") break
    cycle <- cycle + 1
  }
  intcode
}

intcode_output <- function(intcode) intcode$output


# --- Part One ---

# examples

intcode_create(c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)) %>%
  intcode_run(TRUE, TRUE)

intcode_create(c(1102,34915192,34915192,7,4,7,99,0)) %>%
  intcode_run() %>%
  intcode_output() %>%
  as.character() %>% str_length() == 16

intcode_create(c(104,1125899906842624,99)) %>%
  intcode_run() %>%
  intcode_output() %>%
  as.character() == "1125899906842624"

# --- Part One ---
part_one <- dt %>%
  intcode_create() %>%
  intcode_add_input(1) %>%
  intcode_run(TRUE, TRUE)

part_one %>%
  intcode_output()

# --- Part Two ---
library(tictoc)
tic()
part_two <- dt %>%
  intcode_create() %>%
  intcode_add_input(2) %>%
  # this is going to take around 5 minutes on a 4GHz i9...
  intcode_run(TRUE, TRUE)
toc()

part_two %>%
  intcode_output()