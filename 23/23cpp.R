# Here I tried implementing c++ compilation with Rcpp, just to see how fast it would be.
# Part 2 runs in 0.6 seconds. The equivalent code I wrote in pure R runs in 45 seconds.
# That's about 75 times faster.
# Please forgive my orrible c++ code :)

library(dplyr)
library(stringr)
library(Rcpp)

input <- strsplit("137826495", "")[[1]] %>% as.integer
input_part2 <- c(input, 10:1000000) %>% as.integer

input_to_list_of_next_cups <- function(input){
  n_cups <- length(input)
  input[input] <- c(input[2:n_cups], input[1])
  return(as.integer(input))
}

get_next_n_nodes <- function(following_cups, n, starting_cup){
  result = rep(0, n)
  for (i in 1:n) { 
    result[i] <- following_cups[starting_cup]
    starting_cup <- result[i]
  }
  return(result)
}

get_destination_cup <- function(reference_cup, n_cups) {
  if (reference_cup == 1) {return(n_cups)}
  return(reference_cup - 1)
}

cppFunction('NumericVector play_n_rounds(int n, NumericVector following_cups, int reference_cup) {
  int n_cups = following_cups.size();
  int removed_cups1;
  int removed_cups2;
  int removed_cups3;
  int destination_cup;

  for(int i = 0; i < n; ++i) {
    removed_cups1 = following_cups[reference_cup-1];
    removed_cups2 = following_cups[removed_cups1-1];
    removed_cups3 = following_cups[removed_cups2-1];
    
    if (reference_cup == 1) {
      destination_cup = n_cups;
    } else {
      destination_cup = reference_cup - 1;
    }
    while (destination_cup == removed_cups1 || destination_cup == removed_cups2 || destination_cup == removed_cups3) {
      if (destination_cup == 1) {
        destination_cup = n_cups;
      } else {
        destination_cup = destination_cup - 1;
      }
    }
    
    following_cups[reference_cup-1] = following_cups[removed_cups3-1];
    following_cups[removed_cups3-1] = following_cups[destination_cup-1];
    following_cups[destination_cup-1] = removed_cups1;
    reference_cup = following_cups[reference_cup-1];
  }
  return following_cups;
}')


get_first_n_cups <- function(input, n, t, starting_cup) {
  evolved_state <- play_n_rounds(n = t, input_to_list_of_next_cups(input), input[1])
  return(get_next_n_nodes(evolved_state, n, starting_cup))
}

ans1 <- get_first_n_cups(input, n = 8, t = 100, starting_cup = 1)
ans2 <- prod(get_first_n_cups(input_part2, n = 2, t = 10000000, starting_cup =  1))
