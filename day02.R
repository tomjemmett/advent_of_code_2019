# --- Day 2: 1202 Program Alarm ---

library(tidyverse)
  
# --- Part One ---

# 1: adds 2 numbers from position a and b and stores result in position c
# 2: multiplies 2 numbers from position a and b and stores result in position c
# 99: halt

# step ahead 4 positions after an opcode

# read in the input file, split the string into a single vector of characters at
# each "," character, then convert the characters to numerics
dt <- read_file("day02.txt") %>%
  str_split(",", simplify = TRUE) %>%
  as.numeric()

dt

# the problem above assumes 0 based indexing, which is going to be problematic
# in R...

# program_counter keeps track of where we are in the program: it starts at 0
# and increments by 4
program_counter <- 0

dt[program_counter + 1:4]

# lets build a function to handle our problem
# this function is going to be recursive: it's final result is going to be to
# call the function again, until we reach the halt instruction: then it will
# return the data
intcode_computer <- function(data, program_counter = 0) {
  # get the op_code
  op_code <- data[[program_counter + 1]]
  
  # exit if op_code is 99 (halt)
  if (op_code == 99) {
    return (data)
  }
  
  # get the remaining values
  a <- data[[program_counter + 2]]
  b <- data[[program_counter + 3]]
  c <- data[[program_counter + 4]]
  
  # op_codes 1 and 2 behave very similarly: the only difference is what
  # operation is being executed (+ or *). We can seperate the shared logic by
  # assinging the required operation to a function
  if (op_code == 1) {
    fn <- `+`
  } else if (op_code == 2) {
    fn <- `*`
  }
  
  data[[c+1]] <- fn(data[[a+1]], data[[b+1]])
  
  intcode_computer(data, program_counter + 4)
}

# test that the computer works as expected, see the text from the AOC site
intcode_computer(c(1,9,10,3,2,3,11,0,99,30,40,50))
intcode_computer(c(1,0,0,0,99))
intcode_computer(c(2,3,0,3,99))
intcode_computer(c(2,4,4,5,99,0))
intcode_computer(c(1,1,1,4,99,5,6,0,99))

# set the computer's state as instructed in the text from the AOC site
dt_before_1202 <- dt
dt_before_1202[[1+1]] <- 12
dt_before_1202[[2+1]] <- 2

part_one_answer <- intcode_computer(dt_before_1202)[[1]]
part_one_answer



# --- Part Two ---

# this seems like quite a daunting task when you first read it: you have to vary
# two inputs to reach a target value!
# it might make the problem easier though to see what happens if we were to
# first vary the one input, and then see what happens when we vary the second.
# hopefully both inputs are, by themselves, linear. that is, incrementing the
# value by 1 always changes the output by the same amount.

# first, let's set the target value
target_value <- 19690720

# we need to keep replacing the first two values, so let's create a function to
# do this for us
replace_inputs <- function(data, a, b) {
  data[[1+1]] <- a
  data[[2+1]] <- b
  
  return(data)
}

# we can test that it works by checking it works out the right answer for part 1
intcode_computer(replace_inputs(dt, 12, 2))[[1]] == part_one_answer

# what happens when we vary input 1?
diff_a <- map_dbl(1:10,
                  ~intcode_computer(replace_inputs(dt, .x, 2))[[1]]) %>%
  diff()
diff_a
# looks like this value increase the result by 30,000, which we can confirm by:
# (this will only work if executed immediately after the last line)

# little error checking to make sure all values are the same...
stopifnot(diff_a == diff_a[[1]])
# extract just the first value as they are all the same
diff_a <- diff_a[[1]]

# what happens when we vary input 2?
diff_b <- map_dbl(1:10,
                  ~intcode_computer(replace_inputs(dt, 12, .x))[[1]]) %>%
  diff()
diff_b
# this just changes the output by 1.
stopifnot(diff_b == diff_b[[1]])
diff_b <- diff_b[[1]]

# now, the intial value of the intcode_computer has the values we vary set to 0
# and 0. because we know that varying the inputs is linear, we can work out the
# intial value, subtract this from the target value to tell us how far we are
# from the result.
initial_value <- intcode_computer(dt)[[1]]

diff_value <- target_value - initial_value

# the diff_a causes a bigger shift that diff_b, so let's set value_a to be the
# integer part of diff_value / diff_a (using the floor function)
value_a <- floor(diff_value / diff_a)
# then, we can find value_b by getting the remainder after dividing diff_value
# by diff_a (%%, or the modulo operator)
value_b <- diff_value %% diff_a

# here are our values
c(value_a, value_b)

# lets check this works!
intcode_computer(replace_inputs(dt, value_a, value_b))[[1]] == target_value

# now, format the value for submission:
100 * value_a + value_b
