# December 3, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(3, 2022)

find_duplicate <- function(in_str) {
  all_letters <- c(letters, LETTERS)
  len <- nchar(in_str)/2
  comp_1 <- unlist(strsplit(substr(in_str, 1, len), ""))
  comp_2 <- unlist(strsplit(substr(in_str, 1+len, 2*len), ""))
  err <- intersect(comp_1, comp_2)
  return(sum(which(all_letters == err)))
}

sum(purrr::map_int(pzl_data, find_duplicate))

find_badge <- function(in_list, no_grp) {
  all_letters <- c(letters, LETTERS)
  elf_1 <- unlist(strsplit(in_list[1], ""))
  elf_2 <- unlist(strsplit(in_list[2], ""))
  elf_3 <- unlist(strsplit(in_list[3], ""))
  badge <- intersect(elf_1, intersect(elf_2, elf_3))
  return(which(all_letters == badge))
}

groups <- purrr::map(1:100, function(n) rep(n, 3)) %>% unlist %>% factor
sum(purrr::map_int(split(pzl_data, groups), find_badge))