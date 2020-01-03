# --- Day 13: Care Package ---

library(tidyverse)

options(digits = 22)

dt <- read_file("day13.txt") %>%
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
  ctr <- 0
  while (intcode$state != "complete" & ctr < n) {
    ctr <- ctr + 1
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

intcode_run_to_input <- function(intcode) {
  # run until we require input but have no input, or the program is complete
  while(intcode$state != "complete" & (
    length(intcode$input) > 0 |
    intcode$op_list[intcode$program_counter] %% 100 != 3)) {
    intcode <- intcode_run(intcode, n = 1)
  }
  intcode
}

intcode_clear_output <- function(intcode) {
  intcode$output <- numeric(0)
  intcode
}

get_board <- function(ic) {
  output <- intcode_output(ic)
  L <- length(output)
  
  b <- matrix(nrow = max(output[seq(2, L, 3)]),
              ncol = max(output[seq(1, L, 3)]))
  
  score <- 0
  for (i in seq(1, L, 3)) {
    x <- output[i]
    y <- output[i+1]
    t <- output[i+2]
    
    if (x == -1) {
      score <- t
    } else {
      b[y, x] <- t
    }
  }
  
  board <- list(
    score = score,
    board = b
  )
  class(board) <- "arcade_board"
  
  return (board)
}

print.arcade_board <- function(arcade_board) {
  cat("\014")
  arcade_board$board %>%
    apply(1, function(x) {
      paste0(c(" ", "#", "x", "-", "o")[x+1], collapse = "")
    }) %>%
    paste0(collapse = "\n") %>%
    cat("\n")
  cat("\nScore:", arcade_board$score, "\n")
}

# --- Part One ---
ic <- intcode_create(dt)

part_one <- ic %>%
  intcode_run() %>%
  get_board()

sum(part_one$board == 2)

# --- Part Two ---

dt[[1]] <- 2

ic.init <- intcode_create(dt) %>%
  intcode_run_to_input()

play_game <- function(ic) {
  b <- get_board(ic)$board
  ball <- c((1:nrow(b))[apply(b, 1, function(x) any(x == 4))],
            (1:ncol(b))[apply(b, 2, function(x) any(x == 4))])
  
  while(ic$state != "complete") {
    b <- get_board(ic)
    print(b)
    
    input <- readline("jkl: ")
    
    if (input %in% c("j","k","l")) {
      input <- case_when(
        input == "j" ~ -1,
        input == "k" ~  0,
        input == "l" ~  1
      )
      ic <- ic %>%
        intcode_add_input(input) %>%
        intcode_run_to_input()
    }
  }
}

# could this be optimized?
# get the board
# in the loop:
#  clear the output
#  run 3 cycles to interupt
#  update the board

run_game <- function(ic) {
  b <- get_board(ic)
  ball <- c((1:nrow(b$board))[apply(b$board, 1, function(x) any(x == 4))],
            (1:ncol(b$board))[apply(b$board, 2, function(x) any(x == 4))])
  
  paddle <- c((1:nrow(b$board))[apply(b$board, 1, function(x) any(x == 3))],
              (1:ncol(b$board))[apply(b$board, 2, function(x) any(x == 3))])
  
  ctr <- 0
  while(ic$state != "complete") {
    ctr <- ctr + 1
    b <- get_board(ic)
    #print(b)
    
    if (ctr %% 100 == 0) {
      cat("\rRunning: ctr = ", ctr,
          ", score = ", b$score,
          ", remaining = ", sum(b$board == 2),
          sep = "")
    }
    
    old_ball <- ball
    ball <- c((1:nrow(b$board))[apply(b$board, 1, function(x) any(x == 4))],
              (1:ncol(b$board))[apply(b$board, 2, function(x) any(x == 4))])
    d <- ball - old_ball

    # if ball is moving up, try to move paddle towards ball
    if (d[[1]] < 0) {
      input <- sign(d[[2]]) #ball[[2]] - paddle[[2]])
    } else {
      # predict where ball is going to be and move paddle there
      moves <- paddle[[1]] - ball[[1]]
      ball_end <- ball[[2]] + d[[2]]*moves
      input <- sign(ball_end - paddle[[2]])
    }
    
    paddle[[2]] <- paddle[[2]] + input
    
    ic <- ic %>%
      intcode_add_input(input) %>%
      intcode_run_to_input()
  }
  cat("\nComplete\n")
  return(ic)
}

part_two <- ic.init %>%
  intcode_add_input(0) %>%
  run_game() %>%
  get_board()

part_two
