library(tidyr)

input <- readLines("input.txt")
decks <- list("p1" = input[2:(which(input=="")-1)] %>% as.numeric,
              "p2" = input[(which(input=="")+2):length(input)] %>% as.numeric)

update_decks <- function(decks, winning_player) {
  losing_player <- winning_player %% 2 + 1
  winning_deck <- decks[[winning_player]]
  losing_deck <- decks[[losing_player]]
  
  upd_winning_deck <- append(winning_deck, c(winning_deck[1], losing_deck[1]))[-1]
  upd_losing_deck <- losing_deck[-1]
  assign(paste0("upd_deck_p", winning_player), upd_winning_deck)
  assign(paste0("upd_deck_p", losing_player), upd_losing_deck)
  upd_decks <- list("p1" = upd_deck_p1, "p2" = upd_deck_p2)
  
  return(upd_decks)
}

play_combat <- function(decks) {
  while (length(decks[["p1"]]) > 0 & length(decks[["p2"]] > 0)) {
    winning_player <- ifelse(decks[["p1"]][1] > decks[["p2"]][1], 1, 2)
    decks <- update_decks(decks, winning_player)
  }
  return(list(winning_player, decks[[winning_player]]))
}

play_recursive_combat <- function(decks) {
  
  is_already_seen <- function(deck, configurations_seen){
    if (length(configurations_seen) == 0) {return(0)}
    return(paste0(deck, collapse = "-") %in% configurations_seen) 
  }
  
  configurations_seen <- c()
  
  while (length(decks[["p1"]]) > 0 & length(decks[["p2"]]) > 0) {
    if(is_already_seen(decks[["p1"]], configurations_seen)) {
      return(list(c(1), decks[["p1"]]))
    }
    configurations_seen <- append(configurations_seen, paste0(decks[["p1"]], collapse = "-"))
    
    is_next_round_recusive <- length(decks[["p1"]][-1]) >= decks[["p1"]][1] & length(decks[["p2"]][-1]) >= decks[["p2"]][1]
    if(is_next_round_recusive) {
      subgame_decks <- list("p1" = decks[["p1"]][2:(1+decks[["p1"]][1])],
                            "p2" = decks[["p2"]][2:(1+decks[["p2"]][1])])
      highest_card <- max(unlist(subgame_decks))
      if (highest_card %in% subgame_decks[["p1"]]) { # if player 1 has highest card, he can't lose the recursive game
        winning_player <- 1
      } else {
        winning_player <- play_recursive_combat(subgame_decks)[[1]]
      }
    } else { 
      winning_player <- ifelse(decks[["p1"]][1] > decks[["p2"]][1], 1, 2)
    }
    decks <- update_decks(decks, winning_player)
  }
  return(list(winning_player, decks[[winning_player]]))
}

ans1 <- function(decks) {
  combat_results <- play_combat(decks)
  return(sum(length(combat_results[[2]]):1 * unlist(combat_results[[2]])))
}

ans2 <- function(decks) {
  recursive_combat_results <- play_recursive_combat(decks)
  return(sum(length(recursive_combat_results[[2]]):1 * unlist(recursive_combat_results[[2]])))
}

ans1(decks)
ans2(decks)