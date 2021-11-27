require(stringr)

input <- readLines("C:/Users/flavi/Desktop/advent/17/input.txt")
n_iter = 6
input_edge_length <- length(input)

x_max = input_edge_length+2*n_iter
y_max = input_edge_length+2*n_iter
z_max = 1+2*n_iter
w_max = 1+2*n_iter

make_grid <- function(input_edge_length){
  grid <- array(dim=c(x_max,y_max,z_max, w_max))
  grid[7:14, 7:14,
       7, 7] <- (strsplit(readLines("C:/Users/flavi/Desktop/advent/17/input.txt"),"") %>% unlist)
  grid[grid=="."] = 0
  grid[grid=="#"] = 1
  grid[is.na(grid)] = 0
  class(grid) = "numeric"
  return(grid)
}

evolve_grid <- function(grid, steps){
  for(t in 1:steps){
    grid_t0 <- grid
    for(x in 1:x_max){
      for(y in 1:y_max){
        for(z in 1:z_max){
          for(w in 1:w_max){
            is_active = (grid_t0[x,y,z,w]==1)
            if(is_active & !(sum(grid_t0[max((x-1), 1):(min(x+1,x_max)), 
                                         (max(y-1, 1)):(min(y+1,y_max)), 
                                         (max(z-1,1)):(min(z+1,z_max)),
                                         (max(w-1,1)):(min(w+1,w_max))]) %in% 3:4)){
              grid[x,y,z,w] = 0
            }
            if(!is_active & sum(grid_t0[max((x-1), 1):(min(x+1,x_max)), 
                                        (max(y-1, 1)):(min(y+1,y_max)), 
                                        (max(z-1,1)):(min(z+1,z_max)),
                                        (max(w-1,1)):(min(w+1,w_max))]) == 3){
              grid[x,y,z,w] = 1
            }
          }
        }
      }
    }
  }
  return(grid)
}

sum(make_grid(input_edge_length) %>% 
      evolve_grid(n_iter))
