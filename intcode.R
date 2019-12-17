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
  cat("Output length:", length(intcode$output), "\n")
  cat("Input length:", length(intcode$input), "\n")
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
