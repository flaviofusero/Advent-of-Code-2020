library(purrr)

parse_data <- function(){
  input <<- readLines("input.txt")
  ingredients <<- strsplit(input, " \\(contains ") %>% map(1) %>% unlist %>% strsplit("( )|\\)")
  allergens <<- strsplit(input, " \\(contains ") %>% map(2) %>% unlist %>% strsplit("(, )|\\)")
  all_possible_ingredients <<- ingredients %>% unlist %>% unique
  all_possible_allergens <<- allergens %>% unlist %>% unique
  candidates_for_allergen <<- rep(list(""), length(all_possible_allergens)) %>% setNames(all_possible_allergens)
}

make_list_of_candidates <- function(ingredients, allergens, all_possible_allergens){
    candidates_for_allergen <- sapply(
    all_possible_allergens, function(y){
      Reduce(intersect, ingredients[sapply(allergens, function(x){
        y %in% x
      })])})
  return(candidates_for_allergen)
}

lisclean_list_of_candidates <- function(candidates_for_allergen) {
  for (i in 2:length(candidates_for_allergen)) {
    candidates_for_allergen <- candidates_for_allergen[order(sapply(candidates_for_allergen, length))]
    candidates_for_allergen[i:length(candidates_for_allergen)] <- lapply(
      candidates_for_allergen[i:length(candidates_for_allergen)], 
      setdiff,
      Reduce(union, candidates_for_allergen[1:(i-1)])
    )
  }
  return(candidates_for_allergen)
}

make_safe_ingredients_list <- function(candidates_for_allergen) {
  safe_ingredients <- setdiff(all_possible_ingredients, candidates_for_allergen)
  return(safe_ingredients)
}

ans1 <- function() {
  return(sum(sapply(ingredients, function(x){sum(safe_ingredients %in% x)})))
}

ans2 <- function() {
  alphab_allergen_order <- names(candidates_for_allergen) %>% unlist
  canonical_dangerous_ingredients <- candidates_for_allergen[order(alphab_allergen_order)]
  return(paste0(canonical_dangerous_ingredients, collapse=","))
}

parse_data()

candidates_for_allergen <- make_list_of_candidates(ingredients, allergens, all_possible_allergens) %>% 
  clean_list_of_candidates
safe_ingredients <- candidates_for_allergen %>% make_safe_ingredients_list

ans1()
ans2()
