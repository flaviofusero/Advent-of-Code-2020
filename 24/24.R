require(stringr)
require(stringi)
require(data.table)

input <- readLines("C:/Users/flavi/Desktop/advent/24/input.txt")
directions = sapply(sapply(input, str_extract_all, "[ns]?[we]"), table)

get_displacement <- function(direction) { 
  N = sum(direction[names(direction) == "ne"], # sum function ignores invalid values while "+" doesn't
          direction[names(direction) == "nw"]) - 
    sum(direction[names(direction) == "se"],
        direction[names(direction) == "sw"])
  
  E = sum(direction[names(direction) == "e"]) +
    0.5 * sum(direction[names(direction) == "ne"], 
              direction[names(direction) == "se"]) -
    (sum(direction[names(direction) == "w"]) +
       0.5 * sum(direction[names(direction) == "nw"],
                 direction[names(direction) == "sw"]))
  return(list("N" = as.numeric(N), "E" = as.numeric(E)))
}

ans1 <- function(directions) {
  displacements <- lapply(directions, get_displacement)
  freq_table <- setNames(sapply(displacements, paste0, collapse = "/"), NULL) %>% table
  return(sum(freq_table %% 2 == 1))  
}

make_grid <- function(directions, x_max, y_max){
  grid <- array(0, dim=c(x_max,y_max))
  displacements <- lapply(directions, get_displacement) %>% setNames(NULL)
  for(tile in displacements) {
    grid[tile[[1]] + x_max / 2, 2 * tile[[2]] + y_max / 2] = 
      (grid[tile[[1]] + x_max / 2, 2 * tile[[2]] + y_max / 2] + 1) %% 2
  } 
  return(grid)
}

evolve_grid <- function(grid, steps){
  for(t in 1:steps){
    grid_t0 <- grid
    for(x in 1:x_max){
      for(y in 1:y_max){
        if ((x+y) %% 2 == 0) { # having multiplied coordinates by 2, I artificially inserted some cells in the grid
                               # (those with x+y %% 2 == 1) that now I need to disregard
          is_active = (grid_t0[x,y] == 1)
          if(is_active & !(sum(grid_t0[max((x-1), 1):(min(x+1,x_max)), 
                                       (max(y-2, 1)):(min(y+2,y_max))
                                       ]) %in% c(2,3))){
            grid[x,y] = 0
          }
          if(!is_active & sum(grid_t0[max((x-1), 1):(min(x+1,x_max)), 
                                      (max(y-2, 1)):(min(y+2,y_max)) 
                                      ]) == 2){
            grid[x,y] = 1
          }
        }
      }
    }
  }
  return(grid)
}

ans1(directions)
ans2 <- sum(evolve_grid(make_grid(directions, x_max = 200, y_max = 400), steps = 100) == 1)

