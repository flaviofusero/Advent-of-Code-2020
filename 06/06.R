require(data.table)
require(readr)

input <- fread(gsub(",,", "\r\n", gsub("\r\n", ",", read_file("input.txt"))), header = F, fill = T)[, row := .I]

# part 1
input[, num_of_valid := length(Reduce(union, strsplit(c(V1, V2, V3, V4, V5), ""))), by = row][, sum(num_of_valid)]

# part 2
input[input==""] = "abcdefghijklmnopqrstuvxwyz"
input[, num_of_valid := length(Reduce(intersect, strsplit(c(V1, V2, V3, V4, V5), ""))), by = row][, sum(num_of_valid)]