# --- Day 21: Springdroid Adventure ---

library(tidyverse)
library(gtools)
source("intcode.R")

options(digits = 22, scipen=999)

dt <- read_lines("day21.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

# --- Part One ---

ic <- intcode_create(dt) %>%
  intcode_run_to_input()


# create a function that takes a list of strings, converts them to ascii and
# adds it to the intcode computer as input
intcode_add_ascii <- function(ic, speed = c("WALK", "RUN"), strings) {
  x <- c(strings, paste0(speed, "\n")) %>%
    #map_chr(paste, collapse = ",") %>%
    paste(collapse = "\n") %>%
    asc()
  
  # the intcode_add_input expects arguments to be passed to it by ..., so we
  # need to use the lift_dl function from purrr to allow us to pass in a vector
  # instead
  lift_dl(partial(intcode_add_input, ic))(x[,1])
}

# helper function to solve todays problem. Returns the answer, or shows the
# output and returns NA if it fails to find a solution
run <- function(ic, speed = c("WALK", "RUN"), ...) {
  ic <- intcode_add_ascii(ic, speed, list(...)) %>%
    intcode_run()
  
  out <- intcode_output(ic)

  # if the last output is > 127 it can't be an ascii character
  if (out[[length(out)]] > 127) {
    solution <- out[[length(out)]]
  } else {
    cat(chr(out), "\n")
    cat("ctr:", ic$cycles, "\n")
    solution <- as.numeric(NA)
  }
  solution
}

# Let's consider scenarios: We assume that all gaps are less than our jump
# distance. The jump distance is equal to D (if we jump when D is false, then
# we will land in the hole.)

#--------------------------------
# Single gap, no island
#--------------------------------
#   D.DDDDDDDDDDD
#   CC.CCCCCCCCCC
#   BBB.BBBBBBBBB
#   AAAA.AAAAAAAA
#
#   #####.#######
#   ..J...x...... = AB_D
#--------------------------------
#   D..DDDDDDDDDD
#   CC..CCCCCCCCC
#   BBB..BBBBBBBB
#   AAAA..AAAAAAA
#
#   #####..###### = A__D
#   ...J...x..... 
#--------------------------------
#   D...DDDDDDDDDD
#   CC...CCCCCCCCC
#   BBB...BBBBBBBB
#   AAAA...AAAAAAA
#
#   #####...###### = ___D
#   ....J...x.....
#--------------------------------
# Two gaps, island to hop onto
#--------------------------------
#   D.D.D.DDDDDDDD
#   CC.C..CCCCCCCC
#   BBB.B.BBBBBBBB
#   AAAA.A.AAAAAAA  
#
#   #####.#.###### = AB_D -> _BCD
#   ..J...J...x...
#--------------------------------
#   D.D..DDDDDDDDD
#   CC.C..CCCCCCCC
#   BBB.B..BBBBBBB
#   AAAA.A..AAAAAA
#
#   #####.#..##### = AB_D -> __CD
#   ..J...J...x...
#--------------------------------
#   D.D...DDDDDDDD
#   CC.C...CCCCCCC
#   BBB.B...BBBBBB
#   AAAA.A...AAAAA
#
#   #####.#...#### = AB_D -> ___D
#   ..J...J...x...
#--------------------------------
#   D..D.DDDDDDDDD
#   CC..C.CCCCCCCC
#   BBB..B.BBBBBBB
#   AAAA..A.AAAAAA
#
#   #####..#.##### = A__D -> _BCD
#   ...J...J...x..
#--------------------------------
#   D..D..DDDDDDDD
#   CC..C..CCCCCCC
#   BBB..B..BBBBBB
#   AAAA..A..AAAAA
#
#   #####..#..#### = A__D -> __CD
#   ...J...J...x..
#--------------------------------

# In the mapped out scenarios above we always need D TRUE, and we always need at
# least one of A,B,C FALSE.
# We can think of this as:
#  - We need D to be true, we are going to jump D squares. If D is false we will
#    land in a hole
#  - We don't want to jump if there isn't a hole, so if A,B,C are all true then
#    there is no immediate hole to jump over
#  - If D is true, but either A, B or C are false, then there is a hole, and we
#    can jump it, so jump
# That gives us:
#   (!A | !B | !C) & D
# which we could write as
#  !(A & B & C) & D
#
# On further inspection, we can see that in every scenario mapped out we don't
# need to check B, only C and A are important

run(ic,
    "WALK",
    "OR A T",  # T starts as false, so or with A
    #"AND B T", # now and B and T, not actually required
    "AND C T", # now and C and T
    "NOT T J", # invert T into J
    "AND D J") # and D and J

# --- Part Two ---

#---------------------------------------------------
# I..IIIIIIIIIIIIII
# HH..HHHHHHHHHHHHH
# .GG..GGGGGGGGGGGG
# F.FF..FFFFFFFFFFF
# .E.EE..EEEEEEEEEE
# D.D.DD..DDDDDDDDD
# CC.C.CC..CCCCCCCC
# BBB.B.BB..BBBBBBB
# AAAA.A.AA..AAAAAA
#
# #####.#.##..##### _B_DE__HI -> A__DEFGHI 
# ....J...J...x.... (new)
# ..J...J...x...... AB_D_FG__ -> _BC_EFGHI [no jump]
#---------------------------------------------------
# ..IIIIIIIIIIIIIII
# H..HHHHHHHHHHHHHH
# .G..GGGGGGGGGGGGG
# F.F..FFFFFFFFFFFF
# .E.E..EEEEEEEEEEE
# D.D.D..DDDDDDDDDD
# CC.C.C..CCCCCCCCC
# BBB.B.B..BBBBBBBB
# AAAA.A.A..AAAAAAA
#
# #####.#.#..###### _B_D__FGHI -> __CDEFGHI
# ....J...J...x....
# ..J...J...x......
#---------------------------------------------------
# II...IIIIIIIIIIII
# .HH...HHHHHHHHHHH
# G.GG...GGGGGGGGGG
# FF.FF...FFFFFFFFF
# .EE.EE...EEEEEEEE
# D.DD.DD...DDDDDDD
# CC.CC.CC...CCCCCC
# BBB.BB.BB...BBBBB
# AAAA.AA.AA...AAAA
#
# #####.##.##...### AB_DE_GH_ -> A_CD___HI -> ___DEFGHI
# ..J...J...J...x......

# The second jump is the same as part one: the only difference we need to check
# is the first part. The third example shows we need B now. The constant pattern
# in the new inputs is that we need either E or H, let's try changing our check
# to
# !(A & B & C) & D & (E | H)

run(ic,
    "RUN",
    "OR A T",  # T starts as false, so or with A
    "AND B T", # now and B and T, not actually required
    "AND C T", # now and C and T
    "NOT T J", # invert T into J
    "AND D J", # and D and J
    "NOT E T", # set T to NOT E,
    "NOT T T", # then set T to NOT T, this will now be E
    "OR H T",
    "AND T J")

# from reddit, the most minimal example found is below
# (https://www.reddit.com/r/adventofcode/comments/edntkk/2019_day_21_minimal_instructions/fbmcnxh/)
#run(ic,
#    "RUN",
#    "NOT H J",
#    "OR C J",
#    "AND B J",
#    "AND A J",
#    "NOT J J",
#    "AND D J")
