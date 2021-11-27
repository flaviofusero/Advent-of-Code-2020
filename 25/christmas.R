require(tidyr)

input <- readLines("input.txt") %>% as.numeric
value <- 1
subject <- 7
mod <- 20201227

parse_data <- function(input) { 
  return(list("card_key" = input[1], "door_key" = input[2])) 
}

get_loop_size <- function(value, subject, mod, public_key) {
  for (i in 1:20201225) {
    value <- value * subject
    value <- value %% mod
    if (value == public_key) {
      return(i)
    }
  }
  return(-1)
}

get_encryption_key <- function(value, subject, mod, input) {
  card_loop_size <- get_loop_size(value, subject, mod, parse_data(input)[["card_key"]])
  door_loop_size <- get_loop_size(value, subject, mod, parse_data(input)[["door_key"]])
  subject <- parse_data(input)[["card_key"]]
  
  for (i in 1:door_loop_size) {
    value <- value * subject
    value <- value %% mod
  }
  return(value)
}

get_encryption_key(value, subject, mod, input)