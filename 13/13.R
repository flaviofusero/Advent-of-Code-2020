require(data.table)
require(stringr)
require(Rmpfr)

options(scipen=999)

parse_data <- function(input_path){
  earliest_time_of_leave <<- as.numeric(readLines(input_path)[1])
  bus_ids <<- as.numeric(unlist(str_extract_all(readLines(input_path)[2], "[0-9]+")))
  second_input_line <- strsplit(readLines(input_path)[2], ",")[[1]]
  bus_departing_time_offsets <<- (1:length(second_input_line))[second_input_line != "x"]-1
}

ans1 <- function(){
  time_until_next_bus <- (bus_ids - (earliest_time_of_leave %% bus_ids)) %% bus_ids
  earliest_bus_id <- bus_ids[which(time_until_next_bus == min(time_until_next_bus))]
  return(min(time_until_next_bus)*earliest_bus_id)
}

ans2 <- function(){
  # returns chinese remainder theorem with remainders -bus_departing_time_offsets and moduli bus_ids
  # The three following functions are taken from package 'numbers', but slightly repurposed
  # to accept mpfr arguments (integer with arbitrary precision)
  
  extGCD <- function (a, b){
    sign_ab <- sign(c(a, b))
    A <- matrix(c(abs(c(a, b)), 1, 0, 0, 1), nrow = 2, ncol = 3)
    while (A[1, 1] * A[2, 1] != 0) {
      if (A[1, 1] > A[2, 1]) {
        m <- A[1, 1]%/%A[2, 1]
        A[1, ] <- A[1, ] - m * A[2, ]
      }
      else {
        m <- A[2, 1]%/%A[1, 1]
        A[2, ] <- A[2, ] - m * A[1, ]
      }
    }
    if (A[1, 1] == 0) 
      g <- A[2, ]
    else g <- A[1, ]
    g[2:3] <- sign_ab * g[2:3]
    return(g)
  }
  
  modinv <- function(n, m){
    v <- extGCD(n, m)
    if (v[1] == 0 || v[1] > 1) 
      return(NA)
    if (v[2] >= 0) 
      v[2]
    else v[2] + m
  }
  
  chinese_remainder_theorem <- function(a, m){
    a <- mpfr(a, 120)
    m <- mpfr(m, 120)
    n <- length(m)
    M <- prod(m)
    x <- mpfr(0, 120)
    for (i in 1:n) {
      Mmi <- prod(m[-i])
      mmi <- modinv(Mmi, m[i])
      x <- x + a[i] * Mmi * mmi
    }
    return(x%%M)
  }
  return(chinese_remainder_theorem(-bus_departing_time_offsets, bus_ids))
}

parse_data("C:/Users/flavi/Desktop/advent/13/input.txt")
ans1()
ans2()
