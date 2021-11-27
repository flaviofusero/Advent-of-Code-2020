require(data.table)
require(stringr)

input <- fread("input.txt", header = F)

# part 1
input[, id := strtoi(gsub("[BR]", 1, gsub1("[FL]", 0, V1)), base = 2)][, max(id)]

# part 2
sum(input[, min(id)]:input[, max(id)])-input[, sum(id)]