# December 5, 2022

library(purrr)
source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(5, 2022)

stack_move <- function(n, a, b) {
  if(n > length(a)) {
    stop("ERROR: n is larger than the length of a.")
  }
  a_new <- head(a, -n)
  b_new <- c(b, rev(tail(a, n)))
  return(list(a_new, b_new))
}

stack_move(3, c(1,2,3,4,5,6), c(7,8,9))

stack_locs <- which(unlist(strsplit(pzl_data[9], "")) %in% as.character(1:9))

stack_moves <- pzl_data[11:length(pzl_data)]

stack_moves_int <- map(stack_moves, function(x) {gsub(" from | to ", ",", x) %>% 
    gsub("move ", "", .) %>% 
    strsplit(",") %>% 
    unlist %>% 
    as.integer()})

stacks <- list()
for (n in 8:1) {
  layer <- strsplit(pzl_data[n], "") %>% unlist
  stacks <- append(stacks, list(layer[stack_locs]))
}
stacks_t <- stacks %>% transpose

for (n in 1:9) {
  stacks_t[[n]] <- stacks_t[[n]] %>% unlist
  stacks_t[[n]] <- stacks_t[[n]][stacks_t[[n]] != " "]
}
stacks_t

for (int in stack_moves_int) {
  test <- stack_move(int[1], stacks_t[[int[2]]], stacks_t[[int[3]]])
  stacks_t[[int[2]]] <- test[[1]]
  stacks_t[[int[3]]] <- test[[2]]
}

res <- character(0)
for (stack in stacks_t) {
  res <- c(res, tail(stack, 1))
}
paste(res, collapse = "")

#Should work, but doesn't: map_chr(stacks_t, tail(1))

#Part 2:
stack_move_2 <- function(n, a, b) {
  if(n > length(a)) {
    stop("ERROR: n is larger than the length of a.")
  }
  a_new <- head(a, -n)
  b_new <- c(b, tail(a, n))
  return(list(a_new, b_new))
}

stacks_t <- stacks %>% transpose

for (n in 1:9) {
  stacks_t[[n]] <- stacks_t[[n]] %>% unlist
  stacks_t[[n]] <- stacks_t[[n]][stacks_t[[n]] != " "]
}

for (int in stack_moves_int) {
  test <- stack_move_2(int[1], stacks_t[[int[2]]], stacks_t[[int[3]]])
  stacks_t[[int[2]]] <- test[[1]]
  stacks_t[[int[3]]] <- test[[2]]
}

res <- character(0)
for (stack in stacks_t) {
  res <- c(res, tail(stack, 1))
}
paste(res, collapse = "")