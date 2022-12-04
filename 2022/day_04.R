# December 4, 2022

library(purrr)
source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(4, 2022)

find_inclusion <- function(in_cha) {
  ranges <- unlist(strsplit(in_cha, ","))
  pars1 <- as.integer(unlist(strsplit(ranges[1], "-")))
  pars2 <- as.integer(unlist(strsplit(ranges[2], "-")))
  
  if ((pars1[1] >= pars2[1] & pars1[2] <= pars2[2]) | (pars1[1] <= pars2[1] & pars1[2] >= pars2[2])) {
    return(1L)
  } else {
    return(0L)
  }
}

map_int(pzl_data, find_inclusion) %>% sum

find_overlap <- function(in_cha) {
  ranges <- unlist(strsplit(in_cha, ","))
  pars1 <- as.integer(unlist(strsplit(ranges[1], "-")))
  pars2 <- as.integer(unlist(strsplit(ranges[2], "-")))
  
  range1 <- (pars1[1]):(pars1[2])
  range2 <- (pars2[1]):(pars2[2])
  
  if (length(intersect(range1, range2)) > 0) {
    return(1L)
  } else {
    return(0L)
  }
}

map_int(pzl_data, find_overlap) %>% sum

