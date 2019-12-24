# --- Day 22: Slam Shuffle ---

library(tidyverse)
library(gmp)

dt <- read_lines("day22.txt")

fns <- list()

fns$cut <- function(cards, x) {
  x <- as.numeric(x)
  
  if (x >= 0) {
    cx <- c(cards[-(1:x)], cards[1:x])
  } else {
    y <- length(cards)
    x <- y + x + 1
    cx <- c(cards[x:y], cards[-(x:y)])
  }
  
  cx
}

fns$deal <- function(cards, x) {
  if (x == "into new stack") {
    cx <- rev(cards)
  } else {
    y <- length(cards)
    x <- as.numeric(str_sub(x, 15))
    cx <- numeric(y)
    
    p <- 1
    for (i in 1:y) {
      cx[[p]] <- cards[[i]]
      p <- (p + x - 1) %% y + 1
    }
  }
  cx
}

part_one <- function(cards, input) {
  cx <- cards
  for (i in input) {
    ix <- str_split(i, " ", 2)[[1]]
    fn <- fns[[ ix[[1]] ]]
    arg <- ix[[2]]
    
    cx <- fn(cx, arg)
  }
  cx
}

p1 <- part_one(0:10006, dt)

(seq_along(p1) - 1)[p1 == 2019]

# --- Part Two ---

part_two <- function(card_size, iterations, p, input) {
  # these are much bigger numbers that R can usually store, so let's use the gmp
  # library and big integers (bigz).
  m <- as.bigz(card_size)
  n <- as.bigz(iterations)
  
  # we can think of our functions as linear polynomial functions (a*x+b mod m)
  # that is, each function takes a card at position x & moves it to a*x+b mod m 
  # (if our number becomes m, then we wrap it back to 0).
  # we want to find the card that ends up at position x, so we need to work back
  # from x and invert our functions.
  
  # our input gives us a number of functions that we can compose together.
  
  # a little bit of number theory helps with this problem
  
  # note, that m is prime
  # therefore, phi(m) == m - 1
  # (this means that there are m-1 numbers that are relatively prime to m)
  
  # a^(-1) == a^(phi(m)-1) == a^(m-2)
  # (this means we can get the inverse of the value a by raising to the power
  # m-2)
  #   pow.bigz(as.bigz(123, m), -1) == pow.bigz(as.bigz(123, m), m-2)
  
  # a^(-n) == a^(n*(phi(m)-1)) == a^(n*(m-2))
  # this means that if we raise to the negative power n it is the same as if we
  # raise to the power n*(m-2)
  #   pow.bigz(as.bigz(123, m), -23) == pow.bigz(as.bigz(123, m), 23*(m-2))
  
  # start off with initial values for a and b as 1 and 0. We create these as bigz
  # with modulus m
  ab <- list(a = as.bigz("1", m), b = as.bigz("0", m))
  
  # remember that all of these functions will be calculated mod m
  fns <- list(
    "deal with increment" = function(x,ab) list(a =  ab$a*x, b = ab$b*x),
    "deal into new stack" = function(x,ab) list(a = -ab$a,   b = m-1-ab$b),
    "cut"                 = function(x,ab) list(a =  ab$a,   b = ab$b-x)
  )
  
  # iterate through the lines of input and figure out which function it is that
  # we are using for that input
  fnx <- input %>%
    map(str_starts, names(fns)) %>%
    map(~fns[.x][[1]])
  # get the input required for each input, in the case of deal into new stack it
  # will return NA, otherwise the integer
  fna <- input %>%
    str_extract("-?\\d+$") %>%
    as.numeric()

  # first, partially apply the arguments to the function. this will return a new
  # function where the first argument has been set
  ab <- map2(fnx, fna, partial) %>%
    # then, iterate through each input, and run it with the output from the last
    # function. Initialise with the values 1, 0 set above
    reduce(~.y(.x), .init = ab)
  
  # we have now composed our ax+b functions into a single function. As we have
  # composed multiple linear functions together one after another, this will
  # become a geometric series, which can reduce to
  #   a^n.x + b(1-a^n)(1-a)
  
  # we want to figure out what card ends up in position p, so we need to invert
  # the multiplications.
  
  # because we need to use a^(-n) twice, let's pre-calculate it
  an_inv <- pow.bigz(ab$a, -n)
  
  # now calculate the result
  an_inv*p + ab$b*(1-an_inv)*pow.bigz(1-ab$a, m-2)
}

part_two("10007", "1", 1, dt)
part_two("119315717514047", "101741582076661", 2020, dt)
