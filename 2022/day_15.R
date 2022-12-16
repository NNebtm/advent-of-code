# December 15, 2022

source("~/advent-of-code/readXmas.R")

pzl_data <- get_pzl_data(15, 2022)
pzl_test <- c("Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
              "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
              "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
              "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
              "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
              "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
              "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
              "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
              "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
              "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
              "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
              "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
              "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
              "Sensor at x=20, y=1: closest beacon is at x=15, y=3")
library(purrr)
library(tidyr)

pos_list <- strsplit(pzl_data, ": closest beacon is at ") %>%
# pos_list <- strsplit(pzl_test, ": closest beacon is at ") %>%
  map(function(x) gsub("Sensor at ", "", x)) %>% 
  map(function(x) gsub("x=", "", x)) %>% 
  map(function(x) strsplit(x, ", y=") %>% unlist) %>% 
  map(function(x) as.integer(x))

#y_test <- 10
y_test <- 2000000

pos_tbl <- tibble(x_sens = unlist(transpose(pos_list)[[1]]),
                  y_sens = unlist(transpose(pos_list)[[2]]),
                  x_beac = unlist(transpose(pos_list)[[3]]),
                  y_beac = unlist(transpose(pos_list)[[4]])) %>% 
  mutate(dist = abs(x_sens - x_beac) + abs(y_sens - y_beac))

(range_tbl <- pos_tbl %>% 
    mutate(y_test = y_test,
           dist_test = y_sens - y_test) %>% 
    filter(abs(dist_test) <= dist) %>% 
    mutate(x_range = dist - abs(dist_test)) %>% 
    filter(x_range > 0) %>% 
    mutate(xi = x_sens - x_range,
           xf = x_sens + x_range,
           xi0 = xi - min(xi) + 1,
           xf0 = xf - min(xi) + 1))

n_beac <- pos_tbl %>% 
  filter(y_beac == y_test) %>% 
  distinct(x_beac) %>% 
  nrow

range_vec <- rep(0, max(range_tbl$xf0))
for (n in 1:length(range_tbl$xf0)) {
  x_i <- range_tbl$xi0[n]
  x_f <- range_tbl$xf0[n]
  range_vec[x_i:x_f] <- 1
}
sum(range_vec) - n_beac

#part 2

xy_max <- 4000000
# xy_max <- 20

# #Finding the periphery: Way too slow.
# periphery <- function(x, y, r) {
#   top <- c(x+r,y)
#   bot <- c(x-r,y)
#   per_list <- list(top, bot)
#   for (n in 1:r) {
#     per_list <- append(per_list, list(top + n*c(-1, 1)))
#     per_list <- append(per_list, list(top + n*c(-1, -1)))
#     if(n<r) {
#       per_list <- append(per_list, list(bot + n*c(+1, 1)))
#       per_list <- append(per_list, list(bot + n*c(+1, -1)))
#       
#     }
#   }
#   return(per_list)
# }

#Find the places the sensor areas almost overlap, then test those points.
pos_tbl %>% 
  select(-x_beac, -y_beac) %>% 
  mutate(idx = row_number(),
         dist01 = abs(x_sens - x_sens[[1]]) + abs(y_sens - y_sens[[1]]) - (dist + dist[[1]]),
         dist02 = abs(x_sens - x_sens[[2]]) + abs(y_sens - y_sens[[2]]) - (dist + dist[[2]]),
         dist03 = abs(x_sens - x_sens[[3]]) + abs(y_sens - y_sens[[3]]) - (dist + dist[[3]]),
         dist04 = abs(x_sens - x_sens[[4]]) + abs(y_sens - y_sens[[4]]) - (dist + dist[[4]]),
         dist05 = abs(x_sens - x_sens[[5]]) + abs(y_sens - y_sens[[5]]) - (dist + dist[[5]]),
         dist06 = abs(x_sens - x_sens[[6]]) + abs(y_sens - y_sens[[6]]) - (dist + dist[[6]]),
         dist07 = abs(x_sens - x_sens[[7]]) + abs(y_sens - y_sens[[7]]) - (dist + dist[[7]]),
         dist08 = abs(x_sens - x_sens[[8]]) + abs(y_sens - y_sens[[8]]) - (dist + dist[[8]]),
         dist09 = abs(x_sens - x_sens[[9]]) + abs(y_sens - y_sens[[9]]) - (dist + dist[[9]]),
         dist10 = abs(x_sens - x_sens[[10]]) + abs(y_sens - y_sens[[10]]) - (dist + dist[[10]]),
         dist11 = abs(x_sens - x_sens[[11]]) + abs(y_sens - y_sens[[11]]) - (dist + dist[[11]]),
         dist12 = abs(x_sens - x_sens[[12]]) + abs(y_sens - y_sens[[12]]) - (dist + dist[[12]]),
         dist13 = abs(x_sens - x_sens[[13]]) + abs(y_sens - y_sens[[13]]) - (dist + dist[[13]]),
         dist14 = abs(x_sens - x_sens[[14]]) + abs(y_sens - y_sens[[14]]) - (dist + dist[[14]]),
         dist15 = abs(x_sens - x_sens[[15]]) + abs(y_sens - y_sens[[15]]) - (dist + dist[[15]]),
         dist16 = abs(x_sens - x_sens[[16]]) + abs(y_sens - y_sens[[16]]) - (dist + dist[[16]]),
         dist17 = abs(x_sens - x_sens[[17]]) + abs(y_sens - y_sens[[17]]) - (dist + dist[[17]]),
         dist18 = abs(x_sens - x_sens[[18]]) + abs(y_sens - y_sens[[18]]) - (dist + dist[[18]]),
         dist19 = abs(x_sens - x_sens[[19]]) + abs(y_sens - y_sens[[19]]) - (dist + dist[[19]]),
         dist20 = abs(x_sens - x_sens[[20]]) + abs(y_sens - y_sens[[20]]) - (dist + dist[[20]]),
         dist21 = abs(x_sens - x_sens[[21]]) + abs(y_sens - y_sens[[21]]) - (dist + dist[[21]]),
         dist22 = abs(x_sens - x_sens[[22]]) + abs(y_sens - y_sens[[22]]) - (dist + dist[[22]])) %>% 
  filter(if_any(starts_with("dis"), ~ abs(.)<=2)) %>% 
  select(-(dist01:dist02), -dist04, -(dist07:dist08), -(dist10:dist11), -(dist15:dist17), -dist19, -(dist21:dist22))


edge_points <- function(vec_1, vec_2, dist_1, n = 5) {
  diff <- vec_2 - vec_1
  new_point <- ceiling(diff*(dist_1/sum(abs(diff)))) + vec_1
  new_points <- list(new_point)
  if (n > 0) {
    offset <- expand.grid(-n:n, -n:n)
    for (xy in transpose(offset)) {
      new_points <- append(new_points, list(new_point + unname(unlist(xy))))
    }
  }
  return(new_points)
}

list0312 <- edge_points(pos_tbl[3,] %>% select(x_sens, y_sens) %>% unlist, 
                        pos_tbl[12,] %>% select(x_sens, y_sens) %>% unlist,
                        pos_tbl[3,]$dist)

list0506 <- edge_points(pos_tbl[5,] %>% select(x_sens, y_sens) %>% unlist, 
                        pos_tbl[6,] %>% select(x_sens, y_sens) %>% unlist,
                        pos_tbl[5,]$dist)

list0920 <- edge_points(pos_tbl[9,] %>% select(x_sens, y_sens) %>% unlist, 
                        pos_tbl[20,] %>% select(x_sens, y_sens) %>% unlist,
                        pos_tbl[9,]$dist)

list1314 <- edge_points(pos_tbl[13,] %>% select(x_sens, y_sens) %>% unlist, 
                        pos_tbl[14,] %>% select(x_sens, y_sens) %>% unlist,
                        pos_tbl[13,]$dist)

list1318 <- edge_points(pos_tbl[13,] %>% select(x_sens, y_sens) %>% unlist, 
                        pos_tbl[18,] %>% select(x_sens, y_sens) %>% unlist,
                        pos_tbl[13,]$dist)

list_all <- c(list0312, list0506, list0920, list1314, list1318)

map(list_all, function(vec) {
  x = vec[1]
  y = vec[2]
  abs(x - pos_tbl$x_sens) + abs(y - pos_tbl$y_sens)-pos_tbl$dist}) %>% 
  map(function(x) all(x > -10)) %>% 
  unlist %>% which

for (xy in list_all) {
  x <- xy[1]
  y <- xy[2]
  # print(paste(x,y))
  outside_vec <- abs(x - pos_tbl$x_sens) + abs(y - pos_tbl$y_sens) > pos_tbl$dist
  print(sum(!outside_vec))
  if (all(!outside_vec)) {
    print(paste(x, y))
  }
}

# Brute force solution. Does not run in a reasonable time.
# xy_max <- 20
# 
# for (x in 0:xy_max) {
#   for (y in 0:xy_max) {
#     test <- all(abs(x - dist_tbl$x_sens) + abs(y - dist_tbl$y_sens) > dist_tbl$dist)
#     if (test) {
#       print(paste(x, y))
#     }
#   }
# }

# Second brute-force solution, expected to take 48ish hours.
# (start_time <- Sys.time())
# for (n_test in 1:4000) {
#   issue_tbl <- pos_tbl %>% 
#     mutate(y_test = n_test,
#            dist_test = y_sens - y_test) %>% 
#     filter(abs(dist_test) <= dist) %>% 
#     mutate(x_range = dist - abs(dist_test)) %>% 
#     filter(x_range > 0) %>% 
#     mutate(xi = x_sens - x_range,
#            xf = x_sens + x_range) %>% 
#     arrange(xi) %>% 
#     mutate(xfp = cummax(lag(xf, default = 0)),
#            gap = xi > xfp+1) %>% 
#     filter(max(xf) < xy_max | gap)
#   
#   if (nrow(issue_tbl)>0) {
#     print(issue_tbl)
#     break
#   }
# }
# end_time <- Sys.time()
# end_time - start_time

# y_tests <- function(vec, n = 0) {
#   out_vec <- vec
#   if (n>0) {
#     for (m in 1:n)
#       out_vec <- c(out_vec,
#                    vec + m,
#                    vec - m)
#   }
#   out_vec <- unique(out_vec)
#   return(out_vec)
# }
# 
# #Prioritised brute-force
# (start_time <- Sys.time())
# for (n_test in y_tests(pos_tbl$y_sens, 10)) {
#   issue_tbl <- pos_tbl %>%
#     mutate(y_test = n_test,
#            dist_test = y_sens - y_test) %>%
#     filter(abs(dist_test) <= dist) %>%
#     mutate(x_range = dist - abs(dist_test)) %>%
#     filter(x_range > 0) %>%
#     mutate(xi = x_sens - x_range,
#            xf = x_sens + x_range) %>%
#     arrange(xi) %>%
#     mutate(xfp = cummax(lag(xf, default = 0)),
#            gap = xi > xfp+1) %>%
#     filter(max(xf) < xy_max | gap)
#   
#   if (nrow(issue_tbl)>0) {
#     print(issue_tbl)
#     break
#   }
# }
# end_time <- Sys.time()
# end_time - start_time