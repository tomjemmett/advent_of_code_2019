# --- Day 25: Cryostasis ---

library(tidyverse)
library(gtools)
library(tictoc)

source("intcode.R")

options(digits = 22, scipen=999)

# read the input
dt <- read_lines("day25.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# initialize the computer
ic <- intcode_create(dt) %>%
  intcode_run_to_input()

part_one_run <- function(ic, ...) {
  commands <- c(...)
  
  # read in the commands passed as ... and execute them
  for (cmd in commands) {
    ic <- ic %>%
      intcode_clear_output() %>%
      intcode_add_ascii(cmd) %>%
      intcode_run_to_input()
  }
  
  ic
}
# function to allow us to play the game
part_one_play <- function(ic, ...) {
  ic <- part_one_run(ic, c(...))
  
  while (ic$state != "complete") {
    previous_state <- ic
    
    ic <- intcode_ascii_output(ic)
    input <- readline()
    
    if (input == "SAVE" || input == "") {
      cat("SAVING\n")
      
      return(list(ic = previous_state, commands = commands))
    }
    
    commands <- c(commands, input)
    
    ic <- ic %>%
      intcode_add_ascii(input) %>%
      intcode_run_to_input()
  }
  ic <- intcode_ascii_output(ic)
  
  return(list(ic = previous_state, commands = commands))
}

# I solved this by experimentation: I searched the 3 directions available at the
# start and picked up every item along the way. The following items ended up
# causing issues though, so I left them out:
#  * lava
#  * infinite loop
#  * escape pod
#  * photons
#  * giant electromagnet
# we could build a BFS to explore the game, but it's more fun to play :-)
# use part_one_play(ic) to play

# explore south
commands <- list(
  s = c("south",
        "east",
        "take boulder",
        "west",
        "west",
        "east",
        "north"),
  w = c("west",
        "take hypercube",
        "south",
        "north",
        "west",
        "take space law space brochure",
        "west",
        "north",
        "take shell",
        "west",
        "take mug",
        "south",
        "take festive hat",
        "north",
        "east",
        "south",
        "west",
        "east",
        "east",
        "east",
        "east"),
  e = c("east",
        "north",
        "west",
        "north",
        "take whirled peas",
        "west", 
        "west",
        "take astronaut ice cream",
        "south"))

ic <- ic %>%
  # commands are only split for ease of figuring out what was going on when I
  # was playing
  part_one_run(commands$s,
               commands$w,
               commands$e)

# find the list of items we selected from the list of commands
items <- flatten_chr(commands) %>%
  keep(str_starts, "take") %>%
  # remove the "take " from the start
  str_sub(6)

# get all the combinations, though we are skipping the combination of drop
# nothing (assume that this wont work!)
cm <- list()
for (i in 1:8) {
  cms <- combn(items, i)
  for (c in 1:ncol(cms)) {
    cm[[length(cm)+1]] <- cms[,c]
  }
}


# iterate through all the combinations, but start from the centre and work out
# in both directions
i <- 0
for (v in c(0, rep(1:127, each = 2) * c(1,-1)) + 113) {
  i <- i + 1
  # show progress
  cat("\014(",str_pad(v, 3),") ", str_pad(i, 3), "/",length(cm),"\n", sep = "")
  # get the current input
  input <- cm[[v]] %>%
    map_chr(~paste("drop", .x))
  # show what we are doing at this iteration
  paste(">", input, collapse = "\n") %>% cat()
  
  ic.t <- part_one_run(ic, input, "south")

  # if we have found the solution then the computer will have halted
  if (ic.t$state == "complete") {
    # we have found the solution
    cat("\nFound!\n")
    # show the output
    ic.t <- intcode_ascii_output(ic.t, FALSE)
    # break out of the loop
    break()
  }
}

