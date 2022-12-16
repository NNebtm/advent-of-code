# December 16, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(16, 2022)
pzl_data <- c("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
              "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
              "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
              "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
              "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
              "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
              "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
              "Valve HH has flow rate=22; tunnel leads to valve GG",
              "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
              "Valve JJ has flow rate=21; tunnel leads to valve II")


library(igraph)
library(purrr)

two_let_conv <- function(chr) {
  chr2 <- strsplit(chr, "") %>% unlist
  num <- map_int(chr2, function(x) which(LETTERS == x))
  out <- sum(rev(num)*(100^((0:(length(num)-1)))))
  return(out)
}

two_let_conv("AA")

valve_list <- pzl_data %>%
  gsub("Valve ", "", .) %>% 
  gsub(" has flow rate=\\d*; tunnels? leads? to valves? ", ";", .) %>% 
  strsplit(";") %>% 
  map(function(x) c(x[1], unlist(strsplit(x[2], ", ")))) %>% 
  map(function(x) map_dbl(x, two_let_conv))

edges <- valve_list %>% 
  map(function(x) c(rbind(x[1], tail(x, -1)))) %>% 
  unlist

val_vec <- map_dbl(valve_list, function(x) x[1]) %>% sort

vent_list <- pzl_data[grepl("rate=[1-9]", pzl_data)] %>% 
  gsub("Valve ", "", .) %>% 
  gsub(" has flow rate=", ";", .) %>% 
  gsub("; tunnel.*", "", .) %>% 
  strsplit(";") %>%
  map(function(x) c(two_let_conv(x[1]), as.double(x[2])))

vent_vec <- map_dbl(vent_list, head(1))

#Finding all permutations is too big a job
gtools::permutations(n = length(vent_vec), r = 5, v = vent_vec)

d16_graph <- make_graph(edges, directed = TRUE)

paths_from_AA <- distances(graph = d16_graph, v = 101, mode = "out")

paths_from_AA[,vent_vec]

t_max <- 30

tibble("node" = unlist(transpose(vent_list)[[1]]),
       "flow" = unlist(transpose(vent_list)[[2]]),
       "dist_aa" = paths_from_AA[,vent_vec]) %>% 
  mutate(ppot = (t_max - dist_aa - 1) * flow)
