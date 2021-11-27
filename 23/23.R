library(dplyr)
library(stringr)

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
    starting_cup <- following_cups[starting_cup]
  }
  return(result)
}

get_destination_cup <- function(reference_cup, n_cups) {
  if (reference_cup == 1) {return(n_cups)}
  return(reference_cup - 1)
}

play_n_rounds <- function(n, cups) {
  following_cups <- cups[["following_cups"]]
  reference_cup <- cups[["reference_cup"]]
  n_cups <- length(following_cups)
  
  for(i in 1:n) {
    removed_cups1 <- following_cups[reference_cup]
    removed_cups2 <- following_cups[removed_cups1]
    removed_cups3 <- following_cups[removed_cups2]
    
    destination_cup <- get_destination_cup(reference_cup, n_cups)
    while (destination_cup %in% c(removed_cups1,
                                  removed_cups2,
                                  removed_cups3)) {
      destination_cup <- get_destination_cup(destination_cup, n_cups)
    }
    
    following_cups[c(reference_cup,
                     removed_cups3,
                     destination_cup)] <- 
      c(following_cups[removed_cups3],
        following_cups[destination_cup],
        following_cups[reference_cup]) 
    reference_cup <- following_cups[reference_cup]
  }
  
  return(list("following_cups" = following_cups, 
              "refereence_cup" = reference_cup))
}

get_first_n_cups <- function(input, n, t, starting_cup) {
  cups = list("following_cups" = input_to_list_of_next_cups(input), 
              "reference_cup" = input[1])
  evolved_state <- play_n_rounds(n = t, cups)
  return(get_next_n_nodes(evolved_state[["following_cups"]], n, starting_cup))
}

ans1 <- get_first_n_cups(input, n = 8, t = 100, starting_cup = 1)
ans2 <- prod(get_first_n_cups(input_part2, n = 2, t = 10000000, starting_cup =  1))