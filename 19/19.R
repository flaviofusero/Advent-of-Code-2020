require(stringr)

parse_rules <- function(part){
  stopifnot(part %in% 1:2)
  tidy_rules <- trimws(unlist(str_extract_all(input_rules_sorted, " .*")))
  if(part == 2){
    tidy_rules[[9]] <- "42 | 42 8"
    tidy_rules[[12]] <- "42 31 | 42 11 31"
  }
  return(tidy_rules)
}

rule_n_to_regex <- function(n, rules, recursion_depth = 0){
  m = as.numeric(n)+1 
  if(m %in% atomic_rules_indexes) {return(rules[[m]])}
  if(m %in% c(9, 12) & recursion_depth >= 6){return("")}
  if(!(m %in% c(9, 12))){recursion_depth = 0}
  
  rule_halves <- strsplit(rules[[m]], " \\| ") %>% unlist %>%  str_extract_all("[0-9]+")
  regex_rule_halves <- lapply(
    rule_halves, 
    Vectorize(rule_n_to_regex, "n"), 
    rules = rules, 
    recursion_depth = recursion_depth+1
  )
  regex_rule_whole <- lapply(
    regex_rule_halves, 
    function(x){paste0("(", x, ")", collapse = "")}
  )
  collapsed_regex <- gsub("(a)", "a",
                          gsub("(b)", "b", 
                               paste0("(", regex_rule_whole, ")", collapse = "|"),
                               fixed = T
                          ), fixed = T
  )
  
  return(collapsed_regex)}

ans <- function(part){
  rules <- parse_rules(part)
  atomic_rules_indexes <<- which(rules %in% c("a", "b"))
  test_regex <- paste0("^", rule_n_to_regex(0, rules), "$")
  return(sum(sapply(input_test_strings, 
                    function(x){grepl(test_regex, x)}))
  )
}

input <- readLines("C:/Users/flavi/Desktop/advent/19/input.txt")
input_rules_unsorted <- input[1:(which(input=="")-1)]
input_rules_sorted <- input_rules_unsorted[order(as.numeric(sapply(str_extract_all(input_rules_unsorted, "[0-9]+"), `[[`, 1)))]
input_test_strings <- input[(which(input=="")+1):length(input)]

ans(1)
ans(2)