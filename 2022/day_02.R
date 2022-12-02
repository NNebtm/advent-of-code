# December 2, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(2, 2022)

pzl_test <- c("A Y", "B X", "C Z")

point_sum <- 0
for (pair in pzl_data) {
  pair <- strsplit(pair, " ") %>% unlist
  
  elf_point <- which(LETTERS == pair[1])
  let_point <- which(LETTERS == pair[2]) - 23

  if (elf_point == let_point) {
    win_point <- 3
  } else if ((let_point == 1 & elf_point == 3) | (let_point == 2 & elf_point == 1) | (let_point == 3 & elf_point == 2)) {
    win_point <- 6
  } else {
    win_point <- 0
  }
  
  tot_point <- let_point + win_point
  point_sum <- point_sum + tot_point
  
}
point_sum


point_sum_2 <- 0
for (pair in pzl_data) {
  pair <- strsplit(pair, " ") %>% unlist
  
  if (pair[2] == "X") {
    win_point <- 0
    let_point <- which(LETTERS == pair[1]) - 1
  } else if (pair[2] == "Y") {
    win_point <- 3
    let_point <- which(LETTERS == pair[1])
  } else if (pair[2] == "Z") {
    win_point <- 6
    let_point <- which(LETTERS == pair[1]) + 1
  } 
  
  let_point <- let_point %% 3
  
  if (let_point == 0) {
    let_point <- 3
  }
  
  tot_point <- let_point + win_point
  point_sum_2 <- point_sum_2 + tot_point
  
}
point_sum_2

#Solution with map
library(purrr)
library(microbenchmark)

rps_func_1 <- function(pair) {
  pair <- strsplit(pair, " ") %>% unlist
  
  elf_point <- which(LETTERS == pair[1])
  let_point <- which(LETTERS == pair[2]) - 23
  
  if (elf_point == let_point) {
    win_point <- 3
  } else if ((let_point == 1 & elf_point == 3) | (let_point == 2 & elf_point == 1) | (let_point == 3 & elf_point == 2)) {
    win_point <- 6
  } else {
    win_point <- 0
  }
  
  return(let_point + win_point)
}

sum(map_dbl(pzl_data, rps_func_1))

rps_func_2 <- function(pair) {
  pair <- strsplit(pair, " ") %>% unlist
  
  if (pair[2] == "X") {
    win_point <- 0
    let_point <- which(LETTERS == pair[1]) - 1
  } else if (pair[2] == "Y") {
    win_point <- 3
    let_point <- which(LETTERS == pair[1])
  } else if (pair[2] == "Z") {
    win_point <- 6
    let_point <- which(LETTERS == pair[1]) + 1
  } 
  
  let_point <- let_point %% 3
  
  if (let_point == 0) {
    let_point <- 3
  }
  
  return(let_point + win_point)
}

sum(map_dbl(pzl_data, rps_func_2))