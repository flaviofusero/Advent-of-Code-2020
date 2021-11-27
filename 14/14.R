library(data.table)
library(stringr)
library(R.utils)
options(scipen = 100)

# solution to part 2 uses hashed enviroments
# much faster than named lists!

parse_data <- function(){
  n_of_bits <<- 36 
  input <<- fread("input.txt", header = F)[, .("type" = V1, "value" = V3, "row" = .I)]
  input[, mem_address := as.numeric(str_extract(type, "[0-9]+")), row]
  input[, type := gsub("mem.*", "mem", type)]
  input[type == "mem", value := paste0(paste0(rep("0", n_of_bits-max(nchar(intToBin(value)))), collapse=""), intToBin(value))]
  n <<- input[, .N]
}

bin_to_dec <- function(x){
  y <- as.numeric(strsplit(x, "")[[1]])
  sum(y * 2^rev((seq_along(y)-1)))
}

dec_to_bin <- function(num, length = 35) {
  as.character(num %/% (2^(length:0)) %% 2)
}

initialize_mem <- function(){
  mem <- rep(list(NA), input[, max(mem_address, na.rm = T)])
  return(mem)
}

populate_mem_part1 <- function(mem){
  for(i in 1:n){
    if(input[i,type] == "mask"){
      mask = input[i, strsplit(value,"")]
      masked_bits = (1:n_of_bits)[mask != "X"]
    } else{
      mem[[input[i,mem_address]]] = input[i, strsplit(value,"")]
      mem[[input[i,mem_address]]][masked_bits] = mask[masked_bits]
    }
  }
  return(mem)
}

populate_mem_part2 <- function(mem){
  for(i in 1:n){
    if(input[i,type] == "mask"){
      mask = input[i, strsplit(value,"")]
      bits_1 = (1:n_of_bits)[mask == 1]
      bits_X = (1:n_of_bits)[mask == "X"]
    }else{
      value <- bin_to_dec(input[i, value])
      bin_mem_address <- dec_to_bin(input[i,mem_address])
      bin_mem_address[bits_1] <- "1"
      all_combinations_of_01 <- expand.grid(rep(list(0:1), length(bits_X)))
      for(j in 1:nrow(all_combinations_of_01)){
        bin_mem_address[bits_X] = as.character(all_combinations_of_01[j,])
        # mem <- modifyList(mem, setNames(list(value), paste0(bin_mem_address, collapse = "")))
        mem[[paste0(bin_mem_address, collapse = "")]] = value
      }
    }
  }
  return(mem)
}

ans1 <- function(mem){
  return(Reduce("+", mem[!is.na(mem)] %>% lapply(t) %>% lapply(paste0, collapse = "") %>% lapply(bin_to_dec)))
}

ans2 <- function(mem){
  return(Reduce("+", mget(ls(mem2), envir = mem2)))
}

parse_data()

initialize_mem() %>% 
  populate_mem_part1 %>% 
  ans1

populate_mem_part2(new.env(hash = TRUE)) %>% 
  ans2
  