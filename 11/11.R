require(data.table)
require(itertools)
require(dplyr)

flo = 1
tol = 3

input <- fread("input.txt", header=F)[, tstrsplit(V1, "")]
n = input[, .N]
m = length(input[1,])
colnames(input) <- as.character(1:m)

count_neighbors <- function(dt, i, j){
  i_neighborhood <- (abs((1:n)-i) <= 1)
  j_neighborhood <- (abs((1:m)-j) <= 1)
  return(sum(dt[i_neighborhood, ..j_neighborhood] == "#"))
}

evolve_grid <- function(dt){
  dt_evolved <- copy(dt)
  for(i in 1:n){
    for(j in 1:m){
      if(dt[i, ..j] == "L" & count_neighbors(dt, i, j) < flo){
        dt_evolved[i, (j) := "#"]
      }
      if(dt[i, ..j] == "#" & count_neighbors(dt, i, j) > tol+1){
        dt_evolved[i, (j) := "L"]
      }
    }
  }
  return(dt_evolved)
}

find_stable_state <- function(dt){
  while(1){
    dt_evolved <- evolve_grid(dt)

    if(isTRUE(all.equal(dt_evolved, dt))){
      return(dt)
    }
    dt <- copy(dt_evolved)
  }
}

# part 1
print(sum(find_stable_state(input)=="#"))