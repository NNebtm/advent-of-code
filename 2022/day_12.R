# December 12, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(12, 2022)

library(igraph)
library(purrr)

#Turn the map into a graph
pzl_data_0 <- pzl_data %>% 
  gsub("S", "a", .) %>% 
  gsub("E", "z", .)

chr_list <- map(pzl_data_0, function(x) unlist(strsplit(x, split = "")))

num_vec <- map(unlist(chr_list), function(x) which(x == letters)) %>% unlist

#Find all edges
edges <- c()
for (n in seq_along(num_vec)) {
  if (n > 1) {if (num_vec[n] >= num_vec[n-1] - 1) edges <- c(edges, n, n-1)}
  if (n > 77) {if (num_vec[n] >= num_vec[n-77] - 1) edges <- c(edges, n, n-77)}
  if (n <= length(num_vec) - 77) {if (num_vec[n] >= num_vec[n+77] - 1) edges <- c(edges, n, n+77)}
  if (n <= length(num_vec) - 1) {if (num_vec[n] >= num_vec[n+1] - 1) edges <- c(edges, n, n+1)}
}
edges

aoc_graph <- make_graph(edges, directed = TRUE)

#Find start and end point
chr_list_SE <- map(pzl_data, function(x) unlist(strsplit(x, split = "")))

which(unlist(chr_list_SE) == "S")
which(unlist(chr_list_SE) == "E")

#Run shortest path algorithm
to_end <- shortest.paths(graph = aoc_graph, to = 1593, mode = "out") #from = 1541, 
to_end[1541,]

#part two
a_vec <- which(unlist(chr_list) == "a")
