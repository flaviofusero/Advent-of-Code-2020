require(data.table)

input <- sort(fread("C:/Users/flavi/Desktop/advent/01/input.txt")$V1)

solve_day1 <- function(input) {
  for(k in length(input):1){
    i = 1
    j = k-1
    while(j > i){
      target = 2020-input[k]
      if(input[i]+input[j]==target){
        print(input[i]*input[j]*input[k])
        break
      }
      if(input[i]+input[j]<target) {
	    i = i+1
	  }
      else {
	    j = j-1
	  }
    }
  }
}

solve_day1(input)
