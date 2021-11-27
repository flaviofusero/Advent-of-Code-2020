require(data.table)
require(dplyr)
require(zoo)

input <- as.numeric(readLines("input.txt"))
l <- length(input)
w <- 25
target <- 18272118

sum_vij <- array(data = NA, dim = c(l,l))
for(i in 1:l){
  for(j in 1:l){
    sum_vij[i,j] = input[i]+input[j]
  }
}

# part 1
for(i in (w+1):l){
  r = (i-25):(i-1)
  if(!(input[i] %in% t(sum_vij[r,r])[lower.tri(sum_vij[r,r], T)])){
    print(input[i])
    break}
}

# part 2
input.dt <- as.data.table(input)
colnames(input.dt) = "n"

for(i in 2:l){
  rollsums <- frollsum(input.dt, i)[[1]]
  if(target %in% rollsums){
    range = (which(rollsums==target)-i+1):which(rollsums==target)
    print(input.dt[range, min(n)]+input.dt[range, max(n)])
  }
}