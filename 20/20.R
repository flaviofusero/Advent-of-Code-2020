library(stringi)
library(stringr)
library(dplyr)

tiles_input <- readLines("C:/Users/flavi/Desktop/advent/20/input.txt")
monster_input <- readLines("C:/Users/flavi/Desktop/advent/20/monster.txt")

parse_data <- function() {
  n_of_tiles <<- (length(tiles_input)+1)/12
  tile_dim <<- tiles_input[2] %>% nchar
  tile_ids <<- rep(0, n_of_tiles) 
  monster <<- matrix(unlist(str_split(monster_input,"")), ncol = 3)
  monster_size <<- sum(monster=="#")
  monster_length <<- dim(monster)[2]
  monster_height <<- dim(monster)[1]
  
  for (i in 1:n_of_tiles) {
    tile_id_and_contents <- tiles_input[(12*(i-1)+1):(12*i+11)]
    tile_ids[i] <<- as.numeric(stri_sub(tile_id_and_contents[1], -5, -2))
    assign(paste0("tile_", tile_ids[i]), 
           matrix(unlist(str_split(tile_id_and_contents[2:11],"")), ncol=10), 
           envir = .GlobalEnv)
  }
}

flip_tile <- function(tile) {
  return(as.data.frame(tile) %>% rev %>% as.matrix)
}

rotate_tile <- function(tile, degrees){
  stopifnot(degrees %% 90 == 0)
  if (degrees == 0) return(tile)
  for (i in 1:((degrees%%360) / 90)) {
    tile <- apply(t(tile),2,rev)
  }
  return(tile)
}

get_border <- function(tile, cardinal_point, rotation = 0, flipped = F) {
  if (flipped == T) tile <- flip_tile(tile)
  if (rotation != 0) tile <- rotate_tile(tile, rotation)
  
  if (cardinal_point=="N") return(tile[1,])
  if (cardinal_point=="E") return(tile[,tile_dim])
  if (cardinal_point=="S") return(tile[tile_dim,])
  if (cardinal_point=="W") return(tile[,1])
}

get_matching_borders <- function(tile_1, tile_2, is_tile_1_fixed = F, is_tile_2_fixed = F){
  if(identical(tile_1, tile_2)){return(NULL)}
  
  if(is_tile_1_fixed == T & is_tile_2_fixed == T){
    for (cardinal_point in list(c("N", "S"), c("E", "W"), c("S", "N"), c("W", "E"))) {
      if (paste0(get_border(tile_1, cardinal_point[[1]]), collapse="") ==
          paste0(get_border(tile_2, cardinal_point[[2]]), collapse ="")){
        return(list(cardinal_point[[1]], cardinal_point[[2]], 0, F, 0, F))
      }
    }
  } else if (is_tile_1_fixed == F & is_tile_2_fixed == T) {
    for (is_tile_1_flipped in c(F, T)) {
      for (tile_1_rotation in c(0, 90, 180, 270)){
        for (cardinal_point in list(c("N", "S"), c("E", "W"), c("S", "N"), c("W", "E"))) {
          if (paste0(get_border(tile_1, cardinal_point[[1]], rotation = tile_1_rotation, is_tile_1_flipped), collapse="") ==
              paste0(get_border(tile_2, cardinal_point[[2]]), collapse ="")){
            return(list(cardinal_point[[1]], cardinal_point[[2]], tile_1_rotation, is_tile_1_flipped, 0, F))
          }
        } 
      }
    }
  } else {
    for (is_tile_2_flipped in c(F, T)) {
      for (tile_2_rotation in c(0, 90, 180, 270)){
        for (cardinal_point in list(c("N", "S"), c("E", "W"), c("S", "N"), c("W", "E"))) {
          if (paste0(get_border(tile_1, cardinal_point[[1]], F), collapse="") ==
              paste0(get_border(tile_2, cardinal_point[[2]], rotation = tile_2_rotation, is_tile_2_flipped), collapse ="")){
            return(list(cardinal_point[[1]], cardinal_point[[2]], 0, F, tile_2_rotation, is_tile_2_flipped))
          }
        }
      }
    }
  }
  return(NULL)
}

make_tile_neighbors_list <- function(){
  # this function works by magic
  # I have an actual efficient implementation in mind, but I can't be bothered to revisit this.
  # Eventually this works anyway
  visited <- rep(F, n_of_tiles)
  tile_neighbors <<- rep(list(rep(0, 4)), n_of_tiles) %>% lapply(setNames, c("N", "E", "S", "W"))
  is_tile_fixed <<- rep(F, n_of_tiles)
  i = 1
  visited[i] = T
  while (length(which(visited==F)) > 0){
    i <- sample(which(visited==T), 1)
    repeat{
      print(i)
      visited[i] <- T
      for (j in 1:length(tile_ids)) {
        matching_borders <- get_matching_borders(
          get(paste0("tile_",tile_ids[i])),
          get(paste0("tile_",tile_ids[j])),
          is_tile_fixed[i],
          is_tile_fixed[j]
        )
        if(!is.null(matching_borders)) {
          tile_neighbors[[i]][[matching_borders[[1]]]] <- tile_ids[j]
          tile_neighbors[[j]][[matching_borders[[2]]]] <- tile_ids[i]
          is_tile_fixed[i] <<- T
          # is_tile_fixed[j] <<- T
          if (matching_borders[[4]] == T) assign(paste0("tile_",tile_ids[j]), flip_tile(get(paste0("tile_",tile_ids[j]))), envir = .GlobalEnv)
          assign(paste0("tile_",tile_ids[j]), rotate_tile(get(paste0("tile_",tile_ids[j])), matching_borders[[3]]), envir = .GlobalEnv)
          if (matching_borders[[6]] == T) assign(paste0("tile_",tile_ids[j]), flip_tile(get(paste0("tile_",tile_ids[j]))), envir = .GlobalEnv)
          assign(paste0("tile_",tile_ids[j]), rotate_tile(get(paste0("tile_",tile_ids[j])), matching_borders[[5]]), envir = .GlobalEnv)
          if(visited[j] == F){
            i_next = j
          }
        }
      }
      i = i_next
      if(visited[i] == T){break}
    }   
  }
  return(tile_neighbors)
}

make_row <- function(head, cardinal){
  for (i in 1:11) {
    head <-  append(head, tile_neighbors[tile_ids==head[length(head)]][[1]][[cardinal]])
  }
  return(head)
}

make_full_picture <- function(tile_neighbors){
  leftmost_column <- make_row(2081, "S") # 2019 is the top-left corner of my output
  full_grid_tiles_ids <- lapply(leftmost_column, make_row, "E")
  
  full_picture <- Reduce(rbind, lapply(full_grid_tiles_ids, function(y){
    Reduce(cbind, lapply(y, function(x){get(paste0("tile_", x))[2:9, 2:9]}))
  }))
  return(full_picture) 
}

parse_data()

tile_neighbors <- make_tile_neighbors_list()
full_pic <- make_full_picture(tile_neighbors)
full_pic_dim <- dim(full_pic)[1]

ans2 <- function(){
  # if this does not work, try to rotate or flip the monster :)
  # I couldn't be bothered to automate this step...
  for (i in 1:(full_pic_dim-monster_height+1)){
    for (j in 1:(full_pic_dim-monster_length+1)){
      monster_position <- full_pic[i:(i+monster_height-1),j:(j+monster_length-1)][monster=="#"]
      if (sum(monster_position=="#", na.rm=T) == monster_size) {
        full_pic[i:(i+monster_height-1), j:(j+monster_length-1)][monster=="#"] = "O"
      }
    }
  }    
  return(sum(full_pic=="#"))
}

ans2()